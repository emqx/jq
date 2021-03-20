#include "jqerl.h"

static int process_json(jq_state *jq, jv value,
        ErlNifEnv* env, ERL_NIF_TERM ret_list, int flags, int dumpopts) {
  int ret = JQ_ERROR_UNKNOWN;
  jq_start(jq, value, flags);
  jv result;
  while (jv_is_valid(result = jq_next(jq))) {
      ret = JQ_OK;
      printf("---process_json---\n");
      //jv_dump(result, dumpopts);
      jv res_jv_str = jv_dump_string(result, dumpopts);
      printf("---res_jv_str---:\n");
      const char* res_str = jv_string_value(res_jv_str);
      printf("res_str: %s\n", res_str);

      ERL_NIF_TERM binterm;
      int binterm_len = strlen(res_str);
      unsigned char* binterm_data = enif_make_new_binary(env, binterm_len, &binterm);
      memcpy(binterm_data, res_str, binterm_len);
      ret_list = enif_make_list_cell(env, binterm, ret_list);
      printf("---process_json end---\n");
      jv_free(res_jv_str);
  }
  if (jv_invalid_has_msg(jv_copy(result))) {
    // Uncaught jq exception
    jv msg = jv_invalid_get_msg(jv_copy(result));
    jv input_pos = jq_util_input_get_position(jq);
    if (jv_get_kind(msg) == JV_KIND_STRING) {
      fprintf(stderr, "jq: error (at %s): %s\n",
              jv_string_value(input_pos), jv_string_value(msg));
    } else {
      msg = jv_dump_string(msg, 0);
      fprintf(stderr, "jq: error (at %s) (not a string): %s\n",
              jv_string_value(input_pos), jv_string_value(msg));
    }
    ret = JQ_ERROR_UNKNOWN;
    jv_free(input_pos);
    jv_free(msg);
  }
  jv_free(result);
  return ret;
}

static ERL_NIF_TERM make_error_return(ErlNifEnv* env, int err_no) {
    const char* err_msg = err_ret[err_no];
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
        enif_make_atom(env, err_msg));
}

static ERL_NIF_TERM make_ok_return(ErlNifEnv* env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // ----------------------------- init --------------------------------------
    jq_state *jq = NULL;
    int ret = JQ_ERROR_UNKNOWN;
    ERL_NIF_TERM ret_term;
    const char* ret_msg;
    int dumpopts = 512; // JV_PRINT_SPACE1
    jq = jq_init();
    if (jq == NULL) {
        ret = JQ_ERROR_SYSTEM;
        ret_msg = "jq_init: malloc error";
        goto out;
    }

    // --------------------------- read args -----------------------------------
    ErlNifBinary erl_jq_filter, erl_json_text;
    if (!enif_inspect_binary(env, argv[0], &erl_jq_filter) ||
        !enif_inspect_binary(env, argv[1], &erl_json_text)) {
        ret = JQ_ERROR_BADARG;
        ret_msg = "invalid input or filter";
        goto out;
	}

    // ------------------------- parse input json -----------------------------
    jv jv_json_text = jv_parse_sized((const char*)erl_json_text.data, erl_json_text.size);
    if (!jv_is_valid(jv_json_text)) {
        ret = JQ_PARSE_ERROR;
        ret_msg = jv_string_value(jv_invalid_get_msg(jv_json_text));
        goto out;
    }
    printf("----------jv_json_text with dumpopts: %d\n", dumpopts);
    jv_dump(jv_json_text, dumpopts);

    // -------------------------- compile filter -------------------------------
    unsigned char* jq_filter = (unsigned char*) malloc(erl_jq_filter.size + 1);
    memcpy(jq_filter, (const char*) erl_jq_filter.data, erl_jq_filter.size);
    jq_filter[erl_jq_filter.size] = '\0';
    if (!jq_compile(jq, jq_filter)) {
        ret = JQ_ERROR_COMPILE;
        ret_msg = "compile jq filter failed";
        goto out;
    }

    // ---------------------- process json text --------------------------------
    ERL_NIF_TERM ret_list = enif_make_list(env, 0);
    /*TODO: process_raw(jq, jv_json_text, &result, 0, dumpopts)*/
    ret = process_json(jq, jv_json_text, env, ret_list, 0, dumpopts);
    jv_free(jv_json_text);
out:// ----------------------------- release -----------------------------------
    switch (ret) {
        default:
            assert(0 && "invalid ret");
        case JQ_ERROR_UNKNOWN:
        case JQ_ERROR_SYSTEM:
        case JQ_ERROR_BADARG:
        case JQ_ERROR_COMPILE:
        case JQ_PARSE_ERROR: {
            printf("---------error: %s\n", ret_msg);
            ret_term = make_error_return(env, ret);
            break;
        }
        case JQ_OK: {
            ret_term = make_ok_return(env, ret_list);
            break;
        }
    }

    free(jq_filter);
    jq_teardown(&jq);
    return ret_term;
}

static ErlNifFunc nif_funcs[] = {
    {"parse", 2, parse_nif}
};

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}

ERL_NIF_INIT(jqerl, nif_funcs, NULL, NULL, upgrade, NULL)
