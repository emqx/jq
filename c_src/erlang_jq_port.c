/*
 * Port program that processes JSON using jq filter programs
 *
 * The functions for sending and receiving packages in this
 * file are inspired by the functions in the Erlang port
 * tutorial which is available here:
 *
 * https://www.erlang.org/doc/tutorial/c_port.html
 *
 */


#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>

#include "port_nif_common.h"
 
#include "lru.h"

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
static int lru_cache_max_size;
static JQStateCacheEntry_lru jq_state_cache;

static void* jq_port_alloc(size_t size) {
    void * data = malloc(size);
    if (data == NULL) {
        fprintf(stderr, "ERROR: malloc returned NULL (out of memory?)\n");
        exit(1);
    }
    return data;
}


static void erlang_jq_set_filter_program_lru_cache_size(int new_size) {
    lru_cache_max_size = new_size;
}

static int erlang_jq_get_filter_program_lru_cache_size() {
    return lru_cache_max_size;
}

static void erlang_jq_port_process_init() {
    lru_cache_max_size = 500;
    set_erljq_alloc(jq_port_alloc);
    set_erljq_free(free);
    JQStateCacheEntry_lru_init(&jq_state_cache);
}

static void erlang_jq_port_process_destroy() {
    JQStateCacheEntry_lru_destroy(&jq_state_cache);
}


// Returns the jq_state cache for the current thread (creates a new cache
// if the current thread don't already have a cache). 
static JQStateCacheEntry_lru * get_jqstate_cache() {
    if (lru_cache_max_size == 0) {
        return NULL;
    }
    JQStateCacheEntry_lru * cache = &jq_state_cache;
    // The max size can change dynamically
    size_t current_size = JQStateCacheEntry_lru_size(cache);
    if (current_size > lru_cache_max_size) {
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
static jq_state* get_jq_state(
        int* ret,
        char** error_message_wb,
        char* erl_jq_filter,
        int* remove_jq_object) {
    JQStateCacheEntry_lru * cache = get_jqstate_cache();
    JQStateCacheEntry new_entry;
    if (lru_cache_max_size > 0) {
        // Check if there is already a jq filter in the LRU cache
        jqstate_cache_entry_init(&new_entry, erl_jq_filter, &lru_cache_max_size);
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
    jq_state *jq = create_jq_state_common(erl_jq_filter, ret, error_message_wb);
    if (jq == NULL) {
        return NULL;
    }
    if (cache != NULL) {
        // Add new entry to cache
        size_t program_size = strlen(erl_jq_filter) + 1;
        char * filter_program_string = erljq_alloc(program_size);
        memcpy(filter_program_string, erl_jq_filter, program_size);
        new_entry.state = jq;
        new_entry.string = filter_program_string;
        JQStateCacheEntry_lru_add(cache, new_entry);
    } else {
        *remove_jq_object = 1;
    }
    return jq;
}

static int erlang_jq_port_process_json(
        char* erl_jq_filter,
        char* json_text,
        int flags,
        int dumpopts,
        String_dynarr* result_strings,
        char** error_msg_wb) {

    int ret = JQ_OK;
    int remove_jq_object = 0;
    jq_state * jq = get_jq_state(&ret, error_msg_wb, erl_jq_filter, &remove_jq_object);
    if (jq == NULL) {
        return ret;
    }
    ret = process_json_common(
            jq,
            json_text,
            flags,
            dumpopts,
            result_strings,
            error_msg_wb);
    if (remove_jq_object) {
        jq_teardown(&jq);
    }
    return ret;
}

static ssize_t read_exact(byte *buf, size_t len) {
    ssize_t last_read_len;
    size_t got = 0;
    do {
        if ((last_read_len = read(0, buf + got, len - got)) <= 0) {
            return last_read_len;
        }
        got += last_read_len;
    } while (got < len);
    if (record_input) {
        for (size_t i = 0; i < len; i++) {
            fputc(buf[i], record_input_file); 
        }
        fflush(record_input_file);
    }
    return got;
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
    if (read_exact(buf, PACKET_SIZE_LEN) != PACKET_SIZE_LEN) {
        return NULL;
    }
    len = 0;
    for (int i = (PACKET_SIZE_LEN-1); i >= 0; i--) {
        size_t addad_byte = ((size_t)buf[PACKET_SIZE_LEN - (i+1)] << (i*sizeof(byte)));
        len = len | addad_byte;
    }
    byte* command_content = erljq_alloc(len);
    if (read_exact(command_content, len) != len) {
        erljq_free(command_content);
        return NULL;
    }
    return command_content;
}

static int write_packet(byte *buf, size_t len) {
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
        erljq_free(jq_program);
        return false;
    }
    String_dynarr result_strings;
    String_dynarr_init(&result_strings);
    char* error_msg = NULL;
    int res = erlang_jq_port_process_json(
            (char*)jq_program,
            (char*)json_data,
            0,
            512,
            &result_strings,
            &error_msg);
    if (res == JQ_OK) {
        size_t nr_of_result_objects = String_dynarr_size(&result_strings);
        int where = 0;
        if (write_packet((byte*)err_tags[JQ_OK], strlen(err_tags[JQ_OK])) <= 0) {
            goto error_on_write_out_0;
        }
        char buf[64];
        sprintf(buf, "%lu", nr_of_result_objects);
        if (write_packet((byte*)buf, strlen(buf)) <= 0) {
            goto error_on_write_out_0;
        }

        for (where = 0; where < nr_of_result_objects; where++) {
            String result = String_dynarr_item_at(&result_strings, where);
            if (write_packet((byte*)result.string, result.size) <= 0) {
                goto error_on_write_out_0;
            }
            erljq_free(result.string);
        }
        String_dynarr_destroy(&result_strings);
        erljq_free(jq_program);
        erljq_free(json_data);
        return true;
error_on_write_out_0:
        for (where = 0; where < nr_of_result_objects; where++) {
            String result = String_dynarr_item_at(&result_strings, where);
            erljq_free(result.string);
        }
        String_dynarr_destroy(&result_strings);
        erljq_free(jq_program);
        erljq_free(json_data);
        return false;
    } else {
        const char* error_str = "error";
        if (write_packet((byte*)error_str, strlen(error_str)) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_packet((byte*)err_tags[res], strlen(err_tags[res])) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_packet((byte*)error_msg, strlen(error_msg)) <= 0) {
            goto error_on_write_out_1;
        } 
        erljq_free(error_msg);
        erljq_free(jq_program);
        erljq_free(json_data);
        return true;
error_on_write_out_1:
        erljq_free(error_msg);
        erljq_free(jq_program);
        erljq_free(json_data);
        return false;
    }
}

static bool handle_exit() {
    erlang_jq_port_process_destroy();
    if (record_input) {
        fclose(record_input_file);
    }
    const char* exiting_str = "exiting"; 
    if (write_packet((byte*)exiting_str, strlen(exiting_str)) <= 0) {
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
        erljq_free(recording_file_name);
        return false;
    }
    const char* ok_str = "ok";
    if (write_packet((byte*)ok_str, strlen(ok_str)) <= 0) {
        erljq_free(recording_file_name);
        fclose(record_input_file);
        return false;
    }
    erljq_free(recording_file_name);
    record_input = true;
    return true;
}

static bool handle_stop_record_input() {
    if (record_input) {
        fclose(record_input_file);
    }
    const char* ok_str = "ok";
    if (write_packet((byte*)ok_str, strlen(ok_str)) <= 0) {
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
    if (write_packet((byte*)ok_str, strlen(ok_str)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_get_filter_program_lru_cache_size() {
    char buffer[64];
    sprintf(buffer, "%d", erlang_jq_get_filter_program_lru_cache_size());
    if (write_packet((byte*)buffer, strlen(buffer)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_ping() {
    const char* response = "pong";
    if (write_packet((byte*)response, strlen(response)) <= 0) {
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
            erljq_free(command);
            if (!handle_process_json()) {
                goto error_return;
            }
       } else if (strcmp((char*)command, "ping") == 0) {
            erljq_free(command);
            // Used to check if the port program is up and running without any problems
            if (!handle_ping()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "exit") == 0) {
            erljq_free(command);
            // Normal exit cumunicate back that we are exiting
            return !handle_exit();
        } else if (strcmp((char*)command, "set_filter_program_lru_cache_max_size") == 0) {
            erljq_free(command);
            // Normal exit cumunicate back that we are exiting
            if (!handle_set_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "get_filter_program_lru_cache_max_size") == 0) {
            erljq_free(command);
            // Normal exit cumunicate back that we are exiting
            if (!handle_get_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } 
        // Recoring input is a debuging functionality that can be used to
        // replay a port program scenario without starting Erlang
        else if (strcmp((char*)command, "start_record_input") == 0) {
            erljq_free(command);
            if (!handle_start_record_input()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "stop_record_input") == 0) {
            erljq_free(command);
            if (!handle_stop_record_input()) {
                goto error_return;
            }
        }
    }
error_return:
    LOG_PRINT("Exiting (the Erlang port may have stopped or crached\n");
    return 1;
}
