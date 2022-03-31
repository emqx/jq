#include "enif_jq.h"
#include "jv.h"
#include "lru.h"


#include <stdbool.h>
#include <threads.h>

// Thread local variable used by jqstate_cache_entry_shall_evict and
// set when creating a new thread local jq_state cache to a
// value (TODO: make this value configurable)
static _Thread_local long jq_state_cache_max_size = 42;

typedef struct JQStateCacheEntry_lru* JQStateCacheEntry_lru_ptr;
static bool JQStateCacheEntry_lru_ptr_eq(
        JQStateCacheEntry_lru_ptr* o1,
        JQStateCacheEntry_lru_ptr* o2) {

    (void)o1;
    (void)o2;
    // not used
    assert(0);
    return 0; 
}
// Generates structs and functions for a dynamic array (used to hold
// pointers to all jq_state caches so they can be freed)
DECLARE_DYNARR_DS(JQStateCacheEntry_lru_ptr,
                  static,
                  enif_alloc,
                  enif_free,
                  JQStateCacheEntry_lru_ptr_eq,
                  1)

// jq_state cache entry (hash and string is the key and state is the value)
typedef struct {
    size_t hash;
    char* string;
    jq_state* state;
} JQStateCacheEntry;

// Hash function for strings from https://stackoverflow.com/a/7666577
// with small modifications
static size_t hash_str(char *str) {
    size_t hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

// Initialize a cache entry without any value
static void jqstate_cache_entry_init(JQStateCacheEntry* entry, char* string) {
    entry->hash = hash_str(string);    
    entry->string = string;
}

static bool jqstate_cache_entry_eq(JQStateCacheEntry* o1, JQStateCacheEntry* o2) {
    return o1->hash == o2->hash && (strcmp(o1->string, o2->string) == 0); 
}

static size_t jqstate_cache_entry_hash(JQStateCacheEntry* o) {
   return o->hash;
}

static void jqstate_cache_entry_destroy(JQStateCacheEntry* o) {
   jq_teardown(&o->state); 
   enif_free(o->string);
}

// This function is used to decide if the least recently used
// item should be evicted when inserting a new item into the
// cache
static bool jqstate_cache_entry_shall_evict(
        size_t current_cache_size,
        JQStateCacheEntry* value) {
    (void)value;
    return current_cache_size > jq_state_cache_max_size;
}

// Generates structs and functions for an LRU (Least Recently Used) cache.
// The LRU cache is used to cache jq_state records to reduce the risk
// that the same JQ filer program needs to be recompiled frequently.
// Compilation of filter programs is very slow so this greatly improves
// performance. Notice that jq_state is mutable and changes while a
// filter program is executed so it is important that the caches are
// thread local.
DECLARE_LRUCACHE_DS(
        JQStateCacheEntry,
        static,
        enif_alloc,
        enif_free,
        jqstate_cache_entry_eq,
        jqstate_cache_entry_hash,
        jqstate_cache_entry_destroy,
        jqstate_cache_entry_shall_evict)

// Data that is private to this version of the module
typedef struct {
    // The following two fields always have to be placed first
    // in any new version of this library so that they can
    // be found when doing hot upgrading.
    
    // The following field should be increased when a new version
    // of this library is released (that can be hot upgraded to
    // from a previous release).
    int version;
    // The following field should be increased by one when the
    // library is hot upgraded (this is used to set the lock name
    // to an unique name)
    int nr_of_loads_before;

    // The maximum size for the thread local jq_state caches
    size_t lru_cache_max_size;
    // thread local storage key used to find the jq_state cache
    tss_t thread_local_jq_state_lru_cache_key;
    // Dynamic array containing pointers to caches so that they can
    // be freed when the module is unloaded
    JQStateCacheEntry_lru_ptr_dynarr caches;
    // Lock protecting the above filed from concurrent modifications
    ErlNifMutex * lock;
} module_private_data;

// Returns the jq_state cache for the current thread (creates a new cache
// if the current thread don't already have a cache). 
static JQStateCacheEntry_lru * get_jqstate_cache(ErlNifEnv* env) {
    module_private_data* data = enif_priv_data(env);
    JQStateCacheEntry_lru * cache =
        tss_get(data->thread_local_jq_state_lru_cache_key);
    if (cache == NULL) {
        // Create new cache
        cache = JQStateCacheEntry_lru_new();
        // Add new cache to array of caches so it can be freed on unload module
        enif_mutex_lock(data->lock);
        JQStateCacheEntry_lru_ptr_dynarr_push(&data->caches, cache);
        enif_mutex_unlock(data->lock);
        // Store the cache in the thread local storage for this thread
        tss_set(data->thread_local_jq_state_lru_cache_key, cache);
        // Set the max size so it can be seen from jqstate_cache_entry_shall_evict
        jq_state_cache_max_size = data->lru_cache_max_size;
    }
    return cache;
}

// Function to generate an Erlang binary with an error message
static ERL_NIF_TERM make_error_msg_bin(
        ErlNifEnv* env,
        const char* msg,
        size_t msg_size) {

    ERL_NIF_TERM err_msg_term;
    memcpy(enif_make_new_binary(env, msg_size, &err_msg_term), msg, msg_size);
    return err_msg_term;
}

// Returns a jq_state containing compiled version of
// erl_jq_filter if compiling is successful and NULL
// otherwise (the jq_state might get fetched from the
// thread local cache if there is already a jq_state
// for erl_jq_filter)
jq_state* get_jq_state(
        ErlNifEnv* env,
        ERL_NIF_TERM* error_msg_bin_ptr,
        int* ret,
        ErlNifBinary erl_jq_filter) {

    // Make sure the JQ filter is \0 terminated
    ERL_NIF_TERM compile_input;
    memcpy(enif_make_new_binary(
                env,
                erl_jq_filter.size + 1,
                &compile_input),
            erl_jq_filter.data,
            erl_jq_filter.size);
    enif_inspect_binary(env, compile_input, &erl_jq_filter);
    erl_jq_filter.data[erl_jq_filter.size-1] = '\0';
    // Check if there is already a jq filter in the LRU cache
    JQStateCacheEntry_lru * cache = get_jqstate_cache(env);
    JQStateCacheEntry new_entry;
    jqstate_cache_entry_init(&new_entry, (char*)erl_jq_filter.data);
    JQStateCacheEntry * cache_entry =
        JQStateCacheEntry_lru_get(cache, new_entry);
    if (cache_entry != NULL) {
        // Return jq_state from cache
        return cache_entry->state;
    }
    // No entry in the cache so we have to create a new jq_state
    jq_state *jq = NULL;
    jq = jq_init();
    if (jq == NULL) {
        *ret = JQ_ERROR_SYSTEM;
        const char* error_message = "jq_init: Could not initialize jq";
        *error_msg_bin_ptr =
            make_error_msg_bin(env, error_message, strlen(error_message));
        return NULL;
    }
    if (!jq_compile(jq, (char*)erl_jq_filter.data)) {
        *ret = JQ_ERROR_COMPILE;
        const char* error_message = "Compilation of jq filter failed";
        *error_msg_bin_ptr =
            make_error_msg_bin(env, error_message, strlen(error_message));
        jq_teardown(&jq);
        return NULL;
    }
    char * filter_program_string = enif_alloc(erl_jq_filter.size);
    memcpy(filter_program_string, erl_jq_filter.data, erl_jq_filter.size);
    new_entry.state = jq;
    new_entry.string = filter_program_string;
    JQStateCacheEntry_lru_add(cache, new_entry);
    return jq;
}

// Used by the error callback function err_callback
typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM* error_msg_bin_ptr;
} NifEnvAndErrBinPtr;

// Callback given to jq before executing a jq_filter
static void err_callback(void *data, jv err) {
    NifEnvAndErrBinPtr* env_and_msg_bin = data;
    ErlNifEnv* env = env_and_msg_bin->env; 
    ERL_NIF_TERM* error_msg_bin_ptr =
        env_and_msg_bin->error_msg_bin_ptr; 
    if (jv_get_kind(err) != JV_KIND_STRING)
        err = jv_dump_string(err, JV_PRINT_INVALID);
    *error_msg_bin_ptr =
        make_error_msg_bin(env,
                           jv_string_value(err),
                           jv_string_length_bytes(jv_copy(err)));
    jv_free(err);
}

// Process the JSON obejct value using the compiled filter program in the
// given jq_state
static int process_json(
        jq_state *jq,
        jv value,
        ErlNifEnv* env, ERL_NIF_TERM *ret_list, int flags, int dumpopts,
        ERL_NIF_TERM* error_msg_bin_ptr) {

  int ret = JQ_ERROR_UNKNOWN;
  jq_start(jq, value, flags);
  jv result;
  ERL_NIF_TERM list0 = enif_make_list(env, 0);
  while (jv_is_valid(result = jq_next(jq))) {
      ret = JQ_OK;
      jv res_jv_str = jv_dump_string(jv_copy(result), dumpopts);
      const char* res_str = jv_string_value(res_jv_str);

      ERL_NIF_TERM binterm;
      int binterm_sz = strlen(res_str);
      memcpy(enif_make_new_binary(env, binterm_sz, &binterm),
             res_str,
             binterm_sz);
      list0 = enif_make_list_cell(env, binterm, list0);
      jv_free(res_jv_str);
  }
  enif_make_reverse_list(env, list0, ret_list);

  if (jv_invalid_has_msg(jv_copy(result))) {
    // Uncaught jq exception
    jv msg = jv_invalid_get_msg(jv_copy(result));
    if (jv_get_kind(msg) == JV_KIND_STRING) {
        size_t binsz =
            snprintf(NULL, 0, "jq error: %s\n", jv_string_value(msg));
        char* bin_data =
            (char*)enif_make_new_binary(env, binsz, error_msg_bin_ptr);
        snprintf(bin_data, binsz, "jq error: %s\n", jv_string_value(msg));
    } else {
        msg = jv_dump_string(msg, 0);
        size_t binsz = snprintf(NULL, 0, "jq error (not a string): %s\n",
                jv_string_value(msg));
        char* bin_data =
            (char*)enif_make_new_binary(env, binsz, error_msg_bin_ptr);
        snprintf(bin_data,
                 binsz,
                 "jq error (not a string): %s\n",
                 jv_string_value(msg));
    }
    ret = JQ_ERROR_PROCESS;
    jv_free(msg);
  }
  jv_free(result);
  return ret;
}

// Helper function to create an error result term
static ERL_NIF_TERM make_error_return(
        ErlNifEnv* env,
        int err_no,
        ERL_NIF_TERM err_msg_bin) {

    const char* err_tag = err_tags[err_no];
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
             enif_make_tuple2(env, enif_make_atom(env, err_tag), err_msg_bin));
}

// Helper function to create a term containing a success result term
static ERL_NIF_TERM make_ok_return(ErlNifEnv* env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

// NIF function taking a binaries for a filter program and a JSON text
// and returning the result of processing the JSON text with the
// filter program 
static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM error_msg_bin; 
    // ----------------------------- init --------------------------------------
    jq_state *jq = NULL;
    int ret = JQ_ERROR_UNKNOWN;
    ERL_NIF_TERM ret_term;
    int dumpopts = 512; // JV_PRINT_SPACE1

    // --------------------------- read args -----------------------------------
    ErlNifBinary erl_jq_filter;
    ErlNifBinary erl_json_text;
    if (!enif_inspect_binary(env, argv[0], &erl_jq_filter) ||
        !enif_inspect_binary(env, argv[1], &erl_json_text)) {
        ret = JQ_ERROR_BADARG;
        const char* error_message =
            "Expected arguments of type binary but got something else";
        error_msg_bin =
            make_error_msg_bin(env, error_message, strlen(error_message));
        goto out;
    }
    // --------- get jq state and compile filter program if not cached ---------
    jq = get_jq_state(env, &error_msg_bin, &ret, erl_jq_filter);
    if (jq == NULL) {
        goto out;
    }
    // Set error callback here so that it gets the right env
    NifEnvAndErrBinPtr env_and_msg_bin = {
        .env = env,
        .error_msg_bin_ptr = &error_msg_bin
    };
    jq_set_error_cb(jq, err_callback, &env_and_msg_bin);

    // ------------------------- parse input json -----------------------------
    // It is reasonable to assume that jv_parse_sized would not require the 
    // input string to be NULL terminated. Unfortunately this is not the case.
    // Error reporting depends on that the input string is NULL terminated
    // so we have to copy the input to a larger memory block to make sure it
    // is NULL terminated. A binary is used because small binaries are cheap
    // as they can be allocated on the process heap.
    ERL_NIF_TERM json_input;
    memcpy(enif_make_new_binary(env, erl_json_text.size + 1, &json_input),
           erl_json_text.data,
           erl_json_text.size);
    enif_inspect_binary(env, json_input, &erl_json_text);
    erl_json_text.data[erl_json_text.size-1] = '\0';
    jv jv_json_text =
        jv_parse_sized((const char*)erl_json_text.data, erl_json_text.size - 1);
    if (!jv_is_valid(jv_json_text)) {
        ret = JQ_ERROR_PARSE;
        // jv_invalid_get_msg destroys input jv object and returns new jv object
        jv_json_text = jv_invalid_get_msg(jv_json_text);
        error_msg_bin =
            make_error_msg_bin(env,
                               jv_string_value(jv_json_text),
                               jv_string_length_bytes(jv_copy(jv_json_text)));
        goto out;
    }

    // ---------------------- process json text --------------------------------
    ERL_NIF_TERM ret_list = enif_make_list(env, 0);
    /*TODO: process_raw(jq, jv_json_text, &result, 0, dumpopts)*/
    ret = process_json(
            jq,
            jv_copy(jv_json_text),
            env,
            &ret_list,
            0,
            dumpopts,
            &error_msg_bin);

out:// ----------------------------- release -----------------------------------
    switch (ret) {
        default:
            assert(0 && "invalid ret");
        case JQ_ERROR_UNKNOWN:
        case JQ_ERROR_SYSTEM:
        case JQ_ERROR_BADARG:
        case JQ_ERROR_COMPILE:
        case JQ_ERROR_PARSE:
        case JQ_ERROR_PROCESS: {
            ret_term = make_error_return(env, ret, error_msg_bin);
            break;
        }
        case JQ_OK: {
            ret_term = make_ok_return(env, ret_list);
            break;
        }
    }
    // jq_next sometimes frees the input json and sometimes not so it is difficult
    // to keep track of how many copies (ref cnt inc) we have made.
    int ref_cnt = jv_get_refcnt(jv_json_text);
    for(int i = 0; i < ref_cnt; i++) { 
        jv_free(jv_json_text);
    }
    return ret_term;
}

static ErlNifFunc nif_funcs[] = {
    /*
       The parse_nif function seems to be very slow (at least when
       given filter programs that are not in the cache).
       The Erlang VM acts very strangely when it is not scheduled
       on a dirty scheduler (for example, timer:sleep() suspends
       for a much longer time than is should).
    */
    {"parse", 2, parse_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

static int get_int_config(
        ErlNifEnv* caller_env,
        ERL_NIF_TERM load_info,
        char* property_name,
        int* res_wb) {

    ERL_NIF_TERM property_atom = enif_make_atom(
            caller_env,
            property_name);
    ERL_NIF_TERM property_term; 
    if ( ! enif_get_map_value(
            caller_env,
            load_info,
            property_atom,
            &property_term)) {
        // Incorrect load info
        return 1;
    }
    if ( ! enif_get_int(
               caller_env,
               property_term,
               res_wb)) {
        // Incorrect load info
        return 1;
    }
    return 0;
}

static int load_helper(
        ErlNifEnv* caller_env,
        void** priv_data,
        ERL_NIF_TERM load_info,
        int nr_of_loads_before) {

    int filter_program_lru_cache_max_size;
    if ( get_int_config(
                caller_env,
                load_info,
                "filter_program_lru_cache_max_size",
                &filter_program_lru_cache_max_size) ) {
        return 1;
    }
    module_private_data* data = enif_alloc(sizeof(module_private_data));
    data->nr_of_loads_before = data->nr_of_loads_before + 1;
    data->version = 0;
    data->lru_cache_max_size = filter_program_lru_cache_max_size;
    if (thrd_success !=
        tss_create(&data->thread_local_jq_state_lru_cache_key, NULL)) {
        enif_free(data);
        return 1;
    }
    char buffer[128];
    sprintf(buffer, "jq.module_private_data_v%d", nr_of_loads_before);
    data->lock = enif_mutex_create(buffer);
    if (data->lock == NULL) {
        tss_delete(data->thread_local_jq_state_lru_cache_key);
        enif_free(data);
        return 1;
    }
    JQStateCacheEntry_lru_ptr_dynarr_init(&data->caches);
    *priv_data = data;
    return 0;
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info) {
    return load_helper(caller_env, priv_data, load_info, 0);
}

void unload(ErlNifEnv* caller_env, void* priv_data) {
    module_private_data* data = priv_data; 
    size_t nr_of_caches = JQStateCacheEntry_lru_ptr_dynarr_size(&data->caches);
    JQStateCacheEntry_lru_ptr* cache_array =
        JQStateCacheEntry_lru_ptr_dynarr_current_raw_array(&data->caches);
    for (int i = 0; i < nr_of_caches; i++) {
        JQStateCacheEntry_lru_free(cache_array[i]);
    }
    JQStateCacheEntry_lru_ptr_dynarr_destroy(&data->caches);
    tss_delete(data->thread_local_jq_state_lru_cache_key);
    enif_mutex_destroy(data->lock);
    enif_free(data);
}

static int upgrade(
        ErlNifEnv* env,
        void** priv_data,
        void** old_priv_data,
        ERL_NIF_TERM load_info) {

    module_private_data* old_data = *old_priv_data;
    return load_helper(env, priv_data, load_info, old_data->nr_of_loads_before);
}

ERL_NIF_INIT(jq, nif_funcs, load, NULL, upgrade, unload)

