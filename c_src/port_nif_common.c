
// This file contains code that is shared by both the NIF and port
// implementation of the Erlang jq library 
#include "jq.h"
#include "port_nif_common.h"
#include <jv.h>
#include <string.h>

char* err_tags[] = {
    "ok",               // 0
    "jq_err_unknown",   // 1
    "jq_err_system",    // 2
    "jq_err_badarg",    // 3
    "jq_err_compile",   // 4
    "jq_err_parse",     // 5
    "jq_err_process"    // 6
};
static const char* ERR_MSG_COULD_NOT_INIT = "jq_init: Could not initialize jq";
static const char* ERR_MSG_COMPILATION_FAILED = "Compilation of jq filter failed";

void* (*custom_erljq_alloc)(size_t) = NULL;
void (*custom_erljq_free)(void *) = NULL;

void set_erljq_alloc(void* (*my_custom_erljq_alloc)(size_t)) {
    custom_erljq_alloc = my_custom_erljq_alloc;
}

void set_erljq_free(void (*my_custom_erljq_free)(void *)) {
    custom_erljq_free = my_custom_erljq_free;
}

static char* my_strdup(char * input) {
   size_t len = strlen(input);
   char* res = erljq_alloc(len + 1);
   memcpy(res, input, len + 1);
   return res;
}

static char* my_str_concat(char * input1, char * input2) {
   size_t len = snprintf(NULL, 0, "%s\n%s",  input1, input2);
   char* res = erljq_alloc(len + 1);
   snprintf(res, len + 1, "%s%s", input1, input2);
   return res;
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
    if ((*err_ptr->error_msg_wb) == NULL) {
        *err_ptr->error_msg_wb = my_strdup((char*)jv_string_value(err));
    } else {
        char * new_err_str =
            my_str_concat(*err_ptr->error_msg_wb, (char*)jv_string_value(err));
        erljq_free(*err_ptr->error_msg_wb);
        *err_ptr->error_msg_wb = new_err_str;
    }
    jv_free(err);
}

jq_state* create_jq_state_common(
        char* filter_program_str,
        int* ret,
        char** error_message_wb) {

    jq_state *jq = NULL;
    jq = jq_init();
    if (jq == NULL) {
        *ret = JQ_ERROR_SYSTEM;
        *error_message_wb = my_strdup((char*)ERR_MSG_COULD_NOT_INIT);
        return NULL;
    }
    *error_message_wb = NULL;
    // Set error callback here so that it gets the right env
    ErrPtr env_and_msg_bin = {
        .error_msg_wb = error_message_wb
    };
    jq_set_error_cb(jq, err_callback, &env_and_msg_bin);
    // Needed to prevent crach when using include in jv programs
    jq_set_attr(jq, jv_string("JQ_LIBRARY_PATH"), jv_array());
    if (!jq_compile(jq, filter_program_str)) {
        *ret = JQ_ERROR_COMPILE;
        if ((*error_message_wb) == NULL) {
            *error_message_wb = my_strdup((char*)ERR_MSG_COMPILATION_FAILED);
        }
        jq_teardown(&jq);
        return NULL;
    } else if ((*error_message_wb) != NULL) {
        // Ignore reported error if jq_compile succeeded
        erljq_free(*error_message_wb);
        *error_message_wb = NULL;
    }
    return jq;
}


int process_json_common(
        jq_state * jq,
        char* json_text,
        int flags,
        int dumpopts,
        String_dynarr* result_strings,
        char** error_msg_wb) {
    
    int ret = JQ_OK;
    // Set error callback here so that it gets the right env
    *error_msg_wb = NULL;
    ErrPtr env_and_msg_bin = {
        .error_msg_wb = error_msg_wb
    };
    jq_set_error_cb(jq, err_callback, &env_and_msg_bin);
    // Parse JSON
    jv json_jv =
        jv_parse_sized(json_text, strlen(json_text));
    if (!jv_is_valid(json_jv)) {
        ret = JQ_ERROR_PARSE;
        if ((*error_msg_wb) == NULL) {
            // jv_invalid_get_msg destroys input jv object and returns new jv object
            json_jv = jv_invalid_get_msg(json_jv);
            *error_msg_wb = my_strdup((char*)jv_string_value(json_jv));
        }
        jv_free(json_jv);
        return ret;
    }

  jq_start(jq, json_jv, flags);
  jv result;
  while (jv_is_valid(result = jq_next(jq))) {
      ret = JQ_OK;
      jv res_jv_str = jv_dump_string(result, dumpopts);
      const char* res_str = jv_string_value(res_jv_str);
      size_t res_str_size = strlen(res_str);
      String port_res_str;
      port_res_str.size = res_str_size;
      
      port_res_str.string = my_strdup((char*)res_str);
      String_dynarr_push(result_strings, port_res_str);
      jv_free(res_jv_str);
  }

  if (jv_invalid_has_msg(jv_copy(result))) {
    // Uncaught jq exception
    jv msg = jv_invalid_get_msg(jv_copy(result));
    if (jv_get_kind(msg) == JV_KIND_STRING) {
        size_t binsz =
            snprintf(NULL, 0, "jq error: %s\n", jv_string_value(msg)) + 1;
        char* bin_data = erljq_alloc(binsz);
        snprintf(bin_data, binsz, "jq error: %s\n", jv_string_value(msg));
        *error_msg_wb = bin_data;
    } else {
        msg = jv_dump_string(msg, 0);
        size_t binsz = snprintf(NULL, 0, "jq error (not a string): %s\n",
                jv_string_value(msg)) + 1;
        char* bin_data = erljq_alloc(binsz);
        snprintf(bin_data, binsz, "jq error (not a string): %s\n",
                jv_string_value(msg));
        *error_msg_wb = bin_data;
    }
    ret = JQ_ERROR_PROCESS;
    jv_free(msg);
  }
  jv_free(result);
  return ret;
}
