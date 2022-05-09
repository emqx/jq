#include <erl_nif.h>
#include "jv.h"
#include "port_nif_common.h"

#include <stdbool.h>
#include <setjmp.h>

#if __STDC_NO_THREADS__ == 0

#include <threads.h>

#elif JQ_NO_PTHREAD == 0

#include <pthread.h>

#define tss_t pthread_key_t
#define thrd_error 1
#define thrd_success 0
#define tss_create(key, dtor) (pthread_key_create(key, dtor) != 0 ? thrd_error : thrd_success)
#define tss_delete(key) pthread_key_delete(key)
#define tss_get(key) pthread_getspecific(key)
#define tss_set(key, data) (pthread_setspecific(key, data) != 0 ? thrd_error : thrd_success)

#else
// As a last resort we fall back to using the NIF library's thread local
// variables as enif_tsd_key_destroy has the following requirement 
// (quote from https://www.erlang.org/doc/man/erl_driver.html#erl_drv_tsd_set):

// "All thread-specific data using this key in all threads must be cleared (see
// erl_drv_tsd_set) before the call to erl_drv_tsd_key_destroy."

// This makes it very tricky to delete keys when the the jq module is unloaded

#define tss_t ErlNifTSDKey
#define thrd_error 1
#define thrd_success 0
#define tss_create(key, dtor) (enif_tsd_key_create("jq_state_cache", key) != 0 ? thrd_error : thrd_success)
#define tss_get(key) enif_tsd_get(key)
#define tss_set(key, data) enif_tsd_set(key, data)
// Do nothing on delete
#define tss_delete(key) 

#endif

static _Thread_local jmp_buf nomem_handling_jmp_buf;

static void* jq_enif_alloc(size_t size) {
    void * data = enif_alloc(size);
    if (data == NULL) {
        fprintf(stderr, "ERROR: enif_alloc returned NULL (out of memory?)\n");
        longjmp(nomem_handling_jmp_buf, 1);
    }
    return data;
}

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
                  erljq_alloc,
                  erljq_free,
                  JQStateCacheEntry_lru_ptr_eq,
                  1)

// Data that is private to this version of the module
typedef struct {
    // The following two fields always have to be placed first
    // in any new version of this library so that they can
    // be found when doing hot upgrading.
    
    // The following field is set to the version which is given
    // when the library is loaded
    int version;
    // The following field should be increased by one when the
    // library is hot upgraded (this is used to set the lock name
    // to an unique name)
    int nr_of_loads_before;

    // The maximum size for the thread local jq_state caches
    int lru_cache_max_size;
    // thread local storage key used to find the jq_state cache
    tss_t thread_local_jq_state_lru_cache_key;
    // Dynamic array containing pointers to caches so that they can
    // be freed when the module is unloaded
    JQStateCacheEntry_lru_ptr_dynarr caches;
    // Lock protecting the above field from concurrent modifications
    ErlNifMutex * lock;
} module_private_data;


// Returns the jq_state cache for the current thread (creates a new cache
// if the current thread don't already have a cache). 
static JQStateCacheEntry_lru * get_jqstate_cache(
        ErlNifEnv* env, module_private_data* data) {
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
    } else {
        // The max size can change dynamically
        size_t current_size = JQStateCacheEntry_lru_size(cache);
        if (current_size > data->lru_cache_max_size) {
            while (JQStateCacheEntry_lru_evict_if_condition_is_true(cache)) {
                // Continue until we have reduced the size to the max size
            }
        }
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
        ErlNifBinary erl_jq_filter,
        int* remove_jq_object) {

    module_private_data* data = enif_priv_data(env);
    JQStateCacheEntry_lru * cache = get_jqstate_cache(env, data);
    JQStateCacheEntry new_entry;
    if (data->lru_cache_max_size > 0) {
        // Check if there is already a jq filter in the LRU cache
        jqstate_cache_entry_init(
                &new_entry,
                (char*)erl_jq_filter.data,
                &data->lru_cache_max_size);
        JQStateCacheEntry * cache_entry =
            JQStateCacheEntry_lru_get(cache, new_entry);
        if (cache_entry != NULL) {
            // Return jq_state from cache
            return cache_entry->state;
        }
    } else {
        cache = NULL;
    }
    // No entry in the cache so we have to create a new jq_state
    char *error_message;
    jq_state *jq =
        create_jq_state_common((char*)erl_jq_filter.data, ret, &error_message);
    /* jq = jq_init(); */
    if (jq == NULL) {
        *error_msg_bin_ptr =
            make_error_msg_bin(env, error_message, strlen(error_message));
        erljq_free(error_message);
        return NULL;
    }
    if (cache != NULL) {
        // Add new entry to cache
        char * filter_program_string = erljq_alloc(erl_jq_filter.size);
        memcpy(filter_program_string, erl_jq_filter.data, erl_jq_filter.size);
        new_entry.state = jq;
        new_entry.string = filter_program_string;
        JQStateCacheEntry_lru_add(cache, new_entry);
    } else {
        *remove_jq_object = 1;
    }
    return jq;
}


// Process the JSON obejct value using the compiled filter program in the
// given jq_state
static int process_json(
        jq_state *jq,
        char* json_text,
        ErlNifEnv* env, ERL_NIF_TERM *ret_list, int flags, int dumpopts,
        ERL_NIF_TERM* error_msg_bin_ptr) {
    String_dynarr result_strings;
    String_dynarr_init(&result_strings);
    char* error_message;
    int res =
        process_json_common(
                jq,
                json_text,
                flags,
                dumpopts,
                &result_strings,
                &error_message); 
    if (res == JQ_OK) {
        size_t nr_of_result_objects = String_dynarr_size(&result_strings);
        ERL_NIF_TERM list0 = enif_make_list(env, 0);
        for (size_t i = 0; i < nr_of_result_objects; i++) {
            String result = String_dynarr_item_at(&result_strings, i);

            ERL_NIF_TERM binterm;
            int binterm_sz = result.size;
            memcpy(enif_make_new_binary(env, binterm_sz, &binterm),
                    result.string,
                    result.size);
            list0 = enif_make_list_cell(env, binterm, list0);
            erljq_free(result.string);
        }
        String_dynarr_destroy(&result_strings);
        enif_make_reverse_list(env, list0, ret_list);
    } else {
        // We need to write back an error response
        int binsz = strlen(error_message);
        char* bin_data =
            (char*)enif_make_new_binary(env, binsz, error_msg_bin_ptr);
        memcpy(bin_data, error_message, binsz);
        erljq_free(error_message);
    }
    return res;
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
static ERL_NIF_TERM process_json_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM error_msg_bin; 
    // ----------------------------- init --------------------------------------
    jq_state *jq = NULL;
    int remove_jq_object = 0;
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
    if (setjmp(nomem_handling_jmp_buf)) {
        // Give badarg exception as this seems more reasonable than allocating and
        // returning an error tuple when we have run out of memory
        return enif_make_badarg(env);
    }
    // --------- get jq state and compile filter program if not cached ---------
    jq = get_jq_state(env, &error_msg_bin, &ret, erl_jq_filter, &remove_jq_object);
    if (jq == NULL) {
        goto out;
    }
    ERL_NIF_TERM ret_list = enif_make_list(env, 0);
    ret = process_json(
            jq,
            (char*)erl_json_text.data,
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
    if (remove_jq_object) {
        jq_teardown(&jq);
    }
    return ret_term;
}

static ERL_NIF_TERM
set_filter_program_lru_cache_max_size_nif(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[]) {

    int value_to_set;
    if ( ! enif_get_int(
               env,
               argv[0],
               &value_to_set) ) {
        // Incorrect load info
        return enif_make_badarg(env);
    }
    if ( value_to_set < 0) {
        return enif_make_badarg(env);
    }
    module_private_data* data = enif_priv_data(env);
    // Hold lock while doing the change to avoid
    // write data race
    enif_mutex_lock(data->lock);
    data->lru_cache_max_size = value_to_set;
    enif_mutex_unlock(data->lock);
    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM
get_filter_program_lru_cache_max_size_nif(
        ErlNifEnv* env,
        int argc,
        const ERL_NIF_TERM argv[]) {

    module_private_data* data = enif_priv_data(env);
    return enif_make_int(env, data->lru_cache_max_size); 
}

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

    set_erljq_alloc(jq_enif_alloc);
    set_erljq_free(enif_free);

    int filter_program_lru_cache_max_size;
    if ( get_int_config(
                caller_env,
                load_info,
                "filter_program_lru_cache_max_size",
                &filter_program_lru_cache_max_size) ) {
        return 1;
    }
    if (filter_program_lru_cache_max_size < 0) {
        return 1;
    }
    int version;
    if ( get_int_config(
                caller_env,
                load_info,
                "version",
                &version) ) {
        return 1;
    }
    module_private_data* data = enif_alloc(sizeof(module_private_data));
    if (data == NULL) {
        fprintf(stderr, "ERROR: enif_alloc returned NULL (out of memory?)\n");
        return 1;
    }
    data->nr_of_loads_before = nr_of_loads_before + 1;
    data->version = version;
    assert(data->version == VERSION);
    data->lru_cache_max_size = filter_program_lru_cache_max_size;
    if (thrd_success !=
        tss_create(&data->thread_local_jq_state_lru_cache_key, NULL)) {
        erljq_free(data);
        return 1;
    }
    char buffer[128];
    sprintf(buffer, "jq.module_private_data_v%d", nr_of_loads_before);
    data->lock = enif_mutex_create(buffer);
    if (data->lock == NULL) {
        tss_delete(data->thread_local_jq_state_lru_cache_key);
        erljq_free(data);
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
    erljq_free(data);
}

static int upgrade(
        ErlNifEnv* env,
        void** priv_data,
        void** old_priv_data,
        ERL_NIF_TERM load_info) {

    module_private_data* old_data = *old_priv_data;
    return load_helper(env, priv_data, load_info, old_data->nr_of_loads_before);
}

static ErlNifFunc nif_funcs[] = {
    /*
       The process_json_nif function seems to be very slow (at least when
       given filter programs that are not in the cache).
       The Erlang VM acts very strangely when it is not scheduled
       on a dirty scheduler (for example, timer:sleep() suspends
       for a much longer time than is should).
    */
    {"process_json", 2, process_json_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"set_filter_program_lru_cache_max_size", 1, set_filter_program_lru_cache_max_size_nif, 0},
    {"get_filter_program_lru_cache_max_size", 0, get_filter_program_lru_cache_max_size_nif, 0}
};

ERL_NIF_INIT(jq_nif, nif_funcs, load, NULL, upgrade, unload)

