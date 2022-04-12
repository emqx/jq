#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "erlang_jq_port_process.h"

typedef unsigned char byte;
#define PACKET_SIZE_LEN 4

//#define ACTIVE_LOG_PRINT
#ifdef ACTIVE_LOG_PRINT
#define LOG_PRINT(...) do{ fprintf( stderr, __VA_ARGS__ ); } while( false )
#else
#define LOG_PRINT(...) do{ } while ( false )
#endif

static bool record_input = false;
static FILE* record_input_file;

static char* err_tags[] = {
    "ok",               // 0
    "jq_err_unknown",   // 1
    "jq_err_system",    // 2
    "jq_err_badarg",    // 3
    "jq_err_compile",   // 4
    "jq_err_parse",     // 5
    "jq_err_process"    // 6
};

static ssize_t read_exact(byte *buf, size_t len) {
    ssize_t i;
    size_t got = 0;
    do {
        if ((i = read(0, buf + got, len - got)) <= 0) {
            return i;
        }
        got += i;
    } while (got < len);
    if (record_input) {
        for (size_t i = 0; i < len; i++) {
            fputc(buf[i], record_input_file); 
        }
    }
    return len;
}

static ssize_t write_exact(byte *buf, size_t len) {
    ssize_t i;
    size_t wrote = 0;

    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote<len);

    return len;
}

typedef unsigned char byte;

static byte* read_packet() {
    uint64_t len = 0;
    byte buf[PACKET_SIZE_LEN]; 
    if ((len = read_exact(buf, PACKET_SIZE_LEN)) != PACKET_SIZE_LEN) {
        return NULL;
    }
    len = 0;
    for (int i = (PACKET_SIZE_LEN-1); i >= 0; i--) {
        size_t addad_byte = ((size_t)buf[PACKET_SIZE_LEN - (i+1)] << (i*sizeof(byte)));
        len = len | addad_byte;
    }
    byte* command_content = malloc(len);
    if (read_exact(command_content, len) != len) {
        free(command_content);
        return NULL;
    }
    return command_content;
}

static int write_cmd(byte *buf, size_t len) {
    byte li;

    for (int i = 0; i < PACKET_SIZE_LEN; i++) {
        li = (len >> (8 * (PACKET_SIZE_LEN - (i+1)))) & 0xff;
        write_exact(&li, 1);
    }

    return write_exact(buf, len);
}

static bool handle_process_json() {
    byte * jq_program = read_packet();
    if (jq_program == NULL) {
        return false;
    }
    byte * json_data = read_packet();
    if (json_data == NULL) {
        free(jq_program);
        return false;
    }
    PortString_dynarr result_strings;
    PortString_dynarr_init(&result_strings);
    char* error_msg = NULL;
    int res = erlang_jq_port_process_json(
            (char*)jq_program,
            (char*)json_data,
            0,
            512,
            &result_strings,
            &error_msg);
    if (res == JQ_OK) {
        size_t nr_of_result_objects = PortString_dynarr_size(&result_strings);
        int where = 0;
        if (write_cmd((byte*)err_tags[JQ_OK], strlen(err_tags[JQ_OK])) <= 0) {
            goto error_on_write_out_0;
        }
        char buf[64];
        sprintf(buf, "%lu", nr_of_result_objects);
        if (write_cmd((byte*)buf, strlen(buf)) <= 0) {
            goto error_on_write_out_0;
        }

        for (where = 0; where < nr_of_result_objects; where++) {
            PortString result = PortString_dynarr_item_at(&result_strings, where);
            if (write_cmd((byte*)result.string, result.size) <= 0) {
                goto error_on_write_out_0;
            }
            free(result.string);
        }
        PortString_dynarr_destroy(&result_strings);
        free(jq_program);
        free(json_data);
        return true;
error_on_write_out_0:
        for (where = 0; where < nr_of_result_objects; where++) {
            PortString result = PortString_dynarr_item_at(&result_strings, where);
            free(result.string);
        }
        PortString_dynarr_destroy(&result_strings);
        free(jq_program);
        free(json_data);
        return false;
    } else {
        const char* error_str = "error";
        if (write_cmd((byte*)error_str, strlen(error_str)) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_cmd((byte*)err_tags[res], strlen(err_tags[res])) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_cmd((byte*)error_msg, strlen(error_msg)) <= 0) {
            goto error_on_write_out_1;
        } 
        free(error_msg);
        free(jq_program);
        free(json_data);
        return true;
error_on_write_out_1:
        free(error_msg);
        free(jq_program);
        free(json_data);
        return false;
    }
}

static bool handle_exit() {
    erlang_jq_port_process_destroy();
    if (record_input) {
        fclose(record_input_file);
    }
    const char* exiting_str = "exiting"; 
    if (write_cmd((byte*)exiting_str, strlen(exiting_str)) <= 0) {
        return false;
    } else {
        fflush(stdout);
        return true;
    }
}

static bool handle_start_record_input() {
    char* recording_file_name = (char*)read_packet();
    if (recording_file_name == NULL) {
        return false;
    }
    record_input_file = fopen(recording_file_name, "wb");
    if (record_input_file == NULL) {
        free(recording_file_name);
        return false;
    }
    const char* ok_str = "ok";
    if (write_cmd((byte*)ok_str, strlen(ok_str)) <= 0) {
        free(recording_file_name);
        fclose(record_input_file);
        return false;
    }
    free(recording_file_name);
    record_input = true;
    return true;
}

static bool handle_stop_record_input() {
    if (record_input) {
        fclose(record_input_file);
    }
    const char* ok_str = "ok";
    if (write_cmd((byte*)ok_str, strlen(ok_str)) <= 0) {
        return false;
    }
    record_input = false;
    return true;
}

static bool handle_set_filter_program_lru_cache_size() {
    char* size_str = (char*)read_packet();
    if (size_str == NULL) {
        return false;
    }
    int new_lru_size = atoi(size_str);
    erlang_jq_set_filter_program_lru_cache_size(new_lru_size);
    const char* ok_str = "ok";
    if (write_cmd((byte*)ok_str, strlen(ok_str)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_get_filter_program_lru_cache_size() {
    char buffer[64];
    sprintf(buffer, "%d", erlang_jq_get_filter_program_lru_cache_size());
    if (write_cmd((byte*)buffer, strlen(buffer)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_ping() {
    const char* response = "pong";
    if (write_cmd((byte*)response, strlen(response)) <= 0) {
        return false;
    }
    return true;
}


int main() {
    erlang_jq_port_process_init();
    while (true) {
        byte* command = read_packet();
        if (command == NULL) {
            LOG_PRINT("Could not read command\n");
            return 1;
        }
        LOG_PRINT("%s\n", command);
        if (strcmp((char*)command, "process_json") == 0) {
            free(command);
            if (!handle_process_json()) {
                goto error_return;
            }
       } else if (strcmp((char*)command, "ping") == 0) {
            free(command);
            // Used to check if the port program is up and running without any problems
            if (!handle_ping()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "exit") == 0) {
            free(command);
            // Normal exit cumunicate back that we are exiting
            return !handle_exit();
        } else if (strcmp((char*)command, "set_filter_program_lru_cache_max_size") == 0) {
            free(command);
            // Normal exit cumunicate back that we are exiting
            if (!handle_set_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "get_filter_program_lru_cache_max_size") == 0) {
            free(command);
            // Normal exit cumunicate back that we are exiting
            if (!handle_get_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } 
        // Recoring input is a debuging functionality that can be used to
        // replay a port program scenario without starting Erlang
        else if (strcmp((char*)command, "start_record_input") == 0) {
            free(command);
            if (!handle_start_record_input()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "stop_record_input") == 0) {
            free(command);
            if (!handle_stop_record_input()) {
                goto error_return;
            }
        }
    }
error_return:
    LOG_PRINT("Exiting (the Erlang port may have stopped or crached\n");
    return 1;
}
