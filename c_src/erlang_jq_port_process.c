
#include "erlang_jq_port_process.h"

#include "jv.h"
#include "jq.h"
#include "lru.h"

#include <string.h>
#include <stdbool.h>


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
static void jqstate_cache_entry_init(
        JQStateCacheEntry* entry,
        char* string) {

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
    //fprintf(stderr, "TEARDOWN\n");
   jq_teardown(&o->state); 
   free(o->string);
}

// This function is used to decide if the least recently used
// item should be evicted when inserting a new item into the
// cache
static bool jqstate_cache_entry_shall_evict(
        size_t current_cache_size,
        JQStateCacheEntry* value);
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
        malloc,
        free,
        jqstate_cache_entry_eq,
        jqstate_cache_entry_hash,
        jqstate_cache_entry_destroy,
        jqstate_cache_entry_shall_evict)

typedef struct {
    int lru_cache_max_size;
    struct JQStateCacheEntry_lru cache;
} module_private_data;

static module_private_data data;

void erlang_jq_set_filter_program_lru_cache_size(int new_size) {
    data.lru_cache_max_size = new_size;
}

int erlang_jq_get_filter_program_lru_cache_size() {
    return data.lru_cache_max_size;
}

void erlang_jq_port_process_init() {
    data.lru_cache_max_size = 500;
    JQStateCacheEntry_lru_init(&data.cache);
}

void erlang_jq_port_process_destroy() {
    JQStateCacheEntry_lru_destroy(&data.cache);
}

// This function is used to decide if the least recently used
// item should be evicted when inserting a new item into the
// cache
static bool jqstate_cache_entry_shall_evict(
        size_t current_cache_size,
        JQStateCacheEntry* value) {
    return current_cache_size > data.lru_cache_max_size;
}

// Returns the jq_state cache for the current thread (creates a new cache
// if the current thread don't already have a cache). 
static JQStateCacheEntry_lru * get_jqstate_cache() {
    if (data.lru_cache_max_size == 0) {
        return NULL;
    }
    JQStateCacheEntry_lru * cache = &data.cache;
    // The max size can change dynamically
    size_t current_size = JQStateCacheEntry_lru_size(cache);
    if (current_size > data.lru_cache_max_size) {
        while (JQStateCacheEntry_lru_evict_if_condition_is_true(cache)) {
            // Continue until we have reduced the size to the max size
        }
    }
    return cache;
}

// Returns a jq_state containing compiled version of
// erl_jq_filter if compiling is successful and NULL
// otherwise (the jq_state might get fetched from the
// thread local cache if there is already a jq_state
// for erl_jq_filter)
jq_state* get_jq_state(
        int* ret,
        char** error_message_wb,
        char* erl_jq_filter,
        int* remove_jq_object) {
    JQStateCacheEntry_lru * cache = get_jqstate_cache();
    JQStateCacheEntry new_entry;
    if (data.lru_cache_max_size > 0) {
        // Check if there is already a jq filter in the LRU cache
        jqstate_cache_entry_init(&new_entry, erl_jq_filter);
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
    jq_state *jq = NULL;
    jq = jq_init();
    if (jq == NULL) {
        *ret = JQ_ERROR_SYSTEM;
        *error_message_wb = "jq_init: Could not initialize jq";
        return NULL;
    }
    if (!jq_compile(jq, erl_jq_filter)) {
        *ret = JQ_ERROR_COMPILE;
        *error_message_wb = "Compilation of jq filter failed";
        jq_teardown(&jq);
        return NULL;
    }
    if (cache != NULL) {
        // Add new entry to cache
        size_t program_size = strlen(erl_jq_filter) + 1;
        char * filter_program_string = malloc(program_size);
        memcpy(filter_program_string, erl_jq_filter, program_size);
        new_entry.state = jq;
        new_entry.string = filter_program_string;
        JQStateCacheEntry_lru_add(cache, new_entry);
    } else {
        *remove_jq_object = 1;
    }
    return jq;
}

// Used by the error callback function err_callback
typedef struct {
    char** error_msg_wb;
} ErrPtr;

// Callback given to jq before executing a jq_filter
static void err_callback(void *data, jv err) {
    ErrPtr* err_ptr = data;
    if (jv_get_kind(err) != JV_KIND_STRING)
        err = jv_dump_string(err, JV_PRINT_INVALID);
    size_t error_message_len =
        jv_string_length_bytes(jv_copy(err)) + 1;
    *err_ptr->error_msg_wb = malloc(error_message_len);
    memcpy(*err_ptr->error_msg_wb, jv_string_value(err), error_message_len -1);
    (*err_ptr->error_msg_wb)[error_message_len - 1] = '\0';
    jv_free(err);
}

static char* jq_port_strdup(char * input) {
   size_t len = strlen(input);
   char* res = malloc(len + 1);
   memcpy(res, input, len + 1);
   return res;
}

// Process the JSON obejct value using the compiled filter program in the
// given jq_state
int erlang_jq_port_process_json(
        char* erl_jq_filter,
        char* json_text,
        int flags,
        int dumpopts,
        PortString_dynarr* result_strings,
        char** error_msg_wb) {
    
    int ret = JQ_OK;
    int remove_jq_object = 0;
    jq_state * jq = get_jq_state(&ret, error_msg_wb, erl_jq_filter, &remove_jq_object);
    if (jq == NULL) {
        return ret;
    }
    // Set error callback here so that it gets the right env
    ErrPtr env_and_msg_bin = {
        .error_msg_wb = error_msg_wb
    };
    jq_set_error_cb(jq, err_callback, &env_and_msg_bin);


    jv json_jv =
        jv_parse_sized(json_text, strlen(json_text));
    if (!jv_is_valid(json_jv)) {
        ret = JQ_ERROR_PARSE;
        // jv_invalid_get_msg destroys input jv object and returns new jv object
        json_jv = jv_invalid_get_msg(json_jv);
        size_t error_message_len =
        jv_string_length_bytes(jv_copy(json_jv)) + 1;
        *error_msg_wb = malloc(error_message_len);
        memcpy(*error_msg_wb,
                jv_string_value(json_jv),
                error_message_len -1);
        jv_free(json_jv);
        (*error_msg_wb)[error_message_len - 1] = '\0';
        if (remove_jq_object) {
            jq_teardown(&jq);
        }
        return ret;
    }

  jq_start(jq, json_jv, flags);
  jv result;
  while (jv_is_valid(result = jq_next(jq))) {
      ret = JQ_OK;
      jv res_jv_str = jv_dump_string(result, dumpopts);
      const char* res_str = jv_string_value(res_jv_str);
      size_t res_str_size = strlen(res_str);
      PortString port_res_str;
      port_res_str.size = res_str_size;
      
      port_res_str.string = jq_port_strdup((char*)res_str);
      PortString_dynarr_push(result_strings, port_res_str);
      jv_free(res_jv_str);
  }

  if (jv_invalid_has_msg(jv_copy(result))) {
    // Uncaught jq exception
    jv msg = jv_invalid_get_msg(jv_copy(result));
    if (jv_get_kind(msg) == JV_KIND_STRING) {
        size_t binsz =
            snprintf(NULL, 0, "jq error: %s\n", jv_string_value(msg)) + 1;
        char* bin_data = malloc(binsz);
        snprintf(bin_data, binsz, "jq error: %s\n", jv_string_value(msg));
        *error_msg_wb = bin_data;
    } else {
        msg = jv_dump_string(msg, 0);
        size_t binsz = snprintf(NULL, 0, "jq error (not a string): %s\n",
                jv_string_value(msg)) + 1;
        char* bin_data = malloc(binsz);
        snprintf(bin_data, binsz, "jq error (not a string): %s\n",
                jv_string_value(msg));
        *error_msg_wb = bin_data;
    }
    ret = JQ_ERROR_PROCESS;
    jv_free(msg);
  }
  jv_free(result);
  if (remove_jq_object) {
    jq_teardown(&jq);
  }
  return ret;
}
