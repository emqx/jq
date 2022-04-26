
#ifndef PORT_NIF_COMMON_H
#define PORT_NIF_COMMON_H

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "jq.h"

#include "dynarr.h"
#include "lru.h"

enum {
    JQ_OK              =  0,
    JQ_ERROR_UNKNOWN   =  1,
    JQ_ERROR_SYSTEM    =  2,
    JQ_ERROR_BADARG    =  3,
    JQ_ERROR_COMPILE   =  4,
    JQ_ERROR_PARSE     =  5,
    JQ_ERROR_PROCESS   =  6
};

extern char* err_tags[];

extern void* (*custom_erljq_alloc)(size_t);
extern void (*custom_erljq_free)(void *);

void set_erljq_alloc(void* (*my_custom_erljq_alloc)(size_t));
void set_erljq_free(void (*my_custom_erljq_free)(void *));

static inline void* erljq_alloc(size_t size) {
   return custom_erljq_alloc(size); 
}

static inline void erljq_free(void* data) {
   custom_erljq_free(data); 
}

typedef struct String {
    size_t size;
    char * string;
} String;

static bool String_eq(
        String* o1,
        String* o2) {

    (void)o1;
    (void)o2;
    // not used
    assert(0);
    return 0; 
}

DECLARE_DYNARR_DS(String,
                  static,
                  erljq_alloc,
                  erljq_free,
                  String_eq,
                  4)

// Hash function for strings from https://stackoverflow.com/a/7666577
// with small modifications
static size_t hash_str(char *str) {
    size_t hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

// jq_state cache entry (hash and string is the key and state is the value)
typedef struct {
    size_t hash;
    char* string;
    jq_state* state;
    int* max_size_ptr;
} JQStateCacheEntry;

// Initialize a cache entry without any value
static void jqstate_cache_entry_init(
        JQStateCacheEntry* entry,
        char* string,
        int* max_size_ptr) {

    entry->hash = hash_str(string);    
    entry->string = string;
    entry->max_size_ptr = max_size_ptr;
}

static bool jqstate_cache_entry_eq(JQStateCacheEntry* o1, JQStateCacheEntry* o2) {
    return o1->hash == o2->hash && (strcmp(o1->string, o2->string) == 0); 
}

static size_t jqstate_cache_entry_hash(JQStateCacheEntry* o) {
   return o->hash;
}

static void jqstate_cache_entry_destroy(JQStateCacheEntry* o) {
   jq_teardown(&o->state); 
   erljq_free(o->string);
}

// This function is used to decide if the least recently used
// item should be evicted when inserting a new item into the
// cache
static bool jqstate_cache_entry_shall_evict(
        size_t current_cache_size,
        JQStateCacheEntry* value) {
    return current_cache_size > *value->max_size_ptr;
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
        erljq_alloc,
        erljq_free,
        jqstate_cache_entry_eq,
        jqstate_cache_entry_hash,
        jqstate_cache_entry_destroy,
        jqstate_cache_entry_shall_evict)

jq_state* create_jq_state_common(
        char* filter_program_str,
        int* ret,
        char** error_message_wb);

int process_json_common(
        jq_state * jq,
        char* json_text,
        int flags,
        int dumpopts,
        String_dynarr* result_strings,
        char** error_msg_wb); 

#endif // PORT_NIF_COMMON_H
