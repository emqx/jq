#include "enif_jq.h"

typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM* error_msg_bin_ptr;
} NifEnvAndErrBinPtr;

// Forward declaration
static ERL_NIF_TERM make_error_msg_bin(ErlNifEnv* env, const char* msg);

static void err_callback(void *data, jv err) {
    NifEnvAndErrBinPtr* env_and_msg_bin = data;
    ErlNifEnv* env = env_and_msg_bin->env; 
    ERL_NIF_TERM* error_msg_bin_ptr = env_and_msg_bin->error_msg_bin_ptr; 
    if (jv_get_kind(err) != JV_KIND_STRING)
        err = jv_dump_string(err, JV_PRINT_INVALID);
    *error_msg_bin_ptr = make_error_msg_bin(env, jv_string_value(err));
    jv_free(err);
}

static int process_json(jq_state *jq, jv value,
        ErlNifEnv* env, ERL_NIF_TERM *ret_list, int flags, int dumpopts,
        ERL_NIF_TERM* error_msg_bin_ptr) {
  int ret = JQ_ERROR_UNKNOWN;
  jq_start(jq, value, flags);
  jv result;
  ERL_NIF_TERM list0 = enif_make_list(env, 0);
  while (jv_is_valid(result = jq_next(jq))) {
      ret = JQ_OK;
      //jv_dump(result, dumpopts);
      jv res_jv_str = jv_dump_string(jv_copy(result), dumpopts);
      const char* res_str = jv_string_value(res_jv_str);

      ERL_NIF_TERM binterm;
      int binterm_sz = strlen(res_str);
      memcpy(enif_make_new_binary(env, binterm_sz, &binterm), res_str, binterm_sz);
      list0 = enif_make_list_cell(env, binterm, list0);
      jv_free(res_jv_str);
  }
  enif_make_reverse_list(env, list0, ret_list);

  if (jv_invalid_has_msg(jv_copy(result))) {
    // Uncaught jq exception
    jv msg = jv_invalid_get_msg(jv_copy(result));
    if (jv_get_kind(msg) == JV_KIND_STRING) {
        size_t binsz = snprintf(NULL, 0, "jq error: %s\n", jv_string_value(msg));
        char* bin_data = (char*)enif_make_new_binary(env, binsz, error_msg_bin_ptr);
        snprintf(bin_data, binsz, "jq error: %s\n", jv_string_value(msg));
    } else {
        msg = jv_dump_string(msg, 0);
        size_t binsz = snprintf(NULL, 0, "jq error (not a string): %s\n",
                jv_string_value(msg));
        char* bin_data = (char*)enif_make_new_binary(env, binsz, error_msg_bin_ptr);
        snprintf(bin_data, binsz, "jq error (not a string): %s\n",
                jv_string_value(msg));
    }
    ret = JQ_ERROR_PROCESS;
    jv_free(msg);
  }
  jv_free(result);
  return ret;
}

static ERL_NIF_TERM make_error_msg_bin(ErlNifEnv* env, const char* msg) {
    ERL_NIF_TERM err_msg_term;
    int binterm_sz = strlen(msg);
    memcpy(enif_make_new_binary(env, binterm_sz, &err_msg_term), msg, binterm_sz);
    return err_msg_term;
}

static ERL_NIF_TERM make_error_return(ErlNifEnv* env, int err_no, ERL_NIF_TERM err_msg_bin) {
    const char* err_tag = err_tags[err_no];
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
             enif_make_tuple2(env, enif_make_atom(env, err_tag), err_msg_bin));
}

static ERL_NIF_TERM make_ok_return(ErlNifEnv* env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM error_msg_bin; 
    // ----------------------------- init --------------------------------------
    jq_state *jq = NULL;
    int ret = JQ_ERROR_UNKNOWN;
    ERL_NIF_TERM ret_term;
    int dumpopts = 512; // JV_PRINT_SPACE1
    jq = jq_init();
    if (jq == NULL) {
        ret = JQ_ERROR_SYSTEM;
        error_msg_bin = make_error_msg_bin(env, "jq_init: malloc error");
        goto out;
    }
    NifEnvAndErrBinPtr env_and_msg_bin = {
        .env = env,
        .error_msg_bin_ptr = &error_msg_bin
    };
    jq_set_error_cb(jq, err_callback, &env_and_msg_bin);

    // --------------------------- read args -----------------------------------
    ErlNifBinary erl_jq_filter, erl_json_text;
    if (!enif_inspect_binary(env, argv[0], &erl_jq_filter) ||
        !enif_inspect_binary(env, argv[1], &erl_json_text)) {
        ret = JQ_ERROR_BADARG;
        error_msg_bin = make_error_msg_bin(env, "jq_init: malloc error");
        goto out;
    }

    // ------------------------- parse input json -----------------------------
    jv jv_json_text = jv_parse_sized((const char*)erl_json_text.data, erl_json_text.size);
    if (!jv_is_valid(jv_json_text)) {
        ret = JQ_ERROR_PARSE;
        // jv_invalid_get_msg destroys input jv object and returns new jv object
        jv_json_text = jv_invalid_get_msg(jv_json_text);
        error_msg_bin = make_error_msg_bin(env, jv_string_value(jv_json_text));
        goto out;
    }
    //jv_dump(jv_json_text, dumpopts);

    // -------------------------- compile filter -------------------------------
    // jq_compile requires that the input program (filter) is a NULL terminated string.
    // Unfortunately, that forces us to create a new larger container that
    // we can put the program inside. We use an Erlang binary for the container
    // as small binaries can be efficiently allocated on the heap.
    ERL_NIF_TERM compile_input;
    memcpy(enif_make_new_binary(env, erl_jq_filter.size + 1, &compile_input), erl_jq_filter.data, erl_jq_filter.size);
    enif_inspect_binary(env, compile_input, &erl_jq_filter);
    erl_jq_filter.data[erl_jq_filter.size-1] = '\0';
    if (!jq_compile(jq, (char*)erl_jq_filter.data)) {
        ret = JQ_ERROR_COMPILE;
        error_msg_bin = make_error_msg_bin(env, "compile jq filter failed");
        goto out;
    }

    // ---------------------- process json text --------------------------------
    ERL_NIF_TERM ret_list = enif_make_list(env, 0);
    /*TODO: process_raw(jq, jv_json_text, &result, 0, dumpopts)*/
    ret = process_json(jq, jv_copy(jv_json_text), env, &ret_list, 0, dumpopts, &error_msg_bin);

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
    jq_teardown(&jq);
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
       The parse_nif function seems to be very slow.
       The Erlang VM acts very strangely when it is not scheduled
       on a dirty scheduler (for example, timer:sleep() suspends
       for a much longer time than is should).
    */
    {"parse", 2, parse_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT(jq, nif_funcs, NULL, NULL, upgrade, NULL)
