#ifndef JQERL_H
#define JQERL_H

#include <erl_nif.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "jq.h"

extern jv jv_parse(const char* string);
extern int jq_compile(jq_state *, const char*);

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);

enum {
    JQ_OK              =  0,
    JQ_ERROR_UNKNOWN   =  1,
    JQ_ERROR_SYSTEM    =  2,
    JQ_ERROR_BADARG    =  3,
    JQ_ERROR_COMPILE   =  4,
    JQ_PARSE_ERROR     =  5
};

char* err_ret[] = {
    "ok",
    "unknown_error",
    "system_error",
    "bad_argument",
    "compile_failed",
    "parse_error"
};

#endif
