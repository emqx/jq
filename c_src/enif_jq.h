#ifndef ENIFJQ_H
#define ENIFJQ_H

#include <erl_nif.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "jq.h"

extern jv jv_parse(const char* string);
extern int jq_compile(jq_state *jq, const char* str);

//int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);

enum {
    JQ_OK              =  0,
    JQ_ERROR_UNKNOWN   =  1,
    JQ_ERROR_SYSTEM    =  2,
    JQ_ERROR_BADARG    =  3,
    JQ_ERROR_COMPILE   =  4,
    JQ_ERROR_PARSE     =  5,
    JQ_ERROR_PROCESS   =  6
};

char* err_tags[] = {
    "ok",               // 0
    "jq_err_unknown",   // 1
    "jq_err_system",    // 2
    "jq_err_badarg",    // 3
    "jq_err_compile",   // 4
    "jq_err_parse",     // 5
    "jq_err_process"    // 6
};

#endif
