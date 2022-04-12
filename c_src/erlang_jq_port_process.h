#ifndef ERLANG_JQ_PORT_PROCESS_H
#define ERLANG_JQ_PORT_PROCESS_H



#include "dynarr.h"
#include "jq.h"

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>

enum {
    JQ_OK              =  0,
    JQ_ERROR_UNKNOWN   =  1,
    JQ_ERROR_SYSTEM    =  2,
    JQ_ERROR_BADARG    =  3,
    JQ_ERROR_COMPILE   =  4,
    JQ_ERROR_PARSE     =  5,
    JQ_ERROR_PROCESS   =  6
};


typedef struct PortString {
    size_t size;
    char * string;
} PortString;

static bool PortString_eq(
        PortString* o1,
        PortString* o2) {

    (void)o1;
    (void)o2;
    // not used
    assert(0);
    return 0; 
}

DECLARE_DYNARR_DS(PortString,
                  static,
                  malloc,
                  free,
                  PortString_eq,
                  4)

int erlang_jq_port_process_json(
        char* erl_jq_filter,
        char* value,
        int flags,
        int dumpopts,
        PortString_dynarr* result_strings,
        char** error_msg_wb);


void erlang_jq_port_process_init();
void erlang_jq_port_process_destroy();
void erlang_jq_set_filter_program_lru_cache_size(int new_size);
int erlang_jq_get_filter_program_lru_cache_size();

#endif
