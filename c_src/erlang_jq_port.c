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

#define _POSIX_C_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#include "port_nif_common.h"
 
#include "lru.h"

typedef unsigned char byte;
#define PACKET_SIZE_LEN 4

//#define ACTIVE_LOG_PRINT
#ifdef ACTIVE_LOG_PRINT
FILE *debug_file;
#define LOG_PRINT(...) do{ fprintf( debug_file, __VA_ARGS__ );  fflush(debug_file);} while( false )
#else
#define LOG_PRINT(...) do{ } while ( false )
#endif

static bool record_input = false;
static FILE* record_input_file;
static int lru_cache_max_size;
static JQStateCacheEntry_lru jq_state_cache;
int pipe_out;
int pipe_in;

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

static ssize_t read_exact(int file_descriptor, byte *buf, size_t len) {
    ssize_t last_read_len;
    size_t got = 0;
    do {
        if ((last_read_len = read(file_descriptor, buf + got, len - got)) <= 0) {
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

static ssize_t write_exact(int file_descriptor, byte *buf, size_t len) {
    ssize_t i;
    size_t wrote = 0;

    do {
        while ((i = write(file_descriptor, buf+wrote, len-wrote)) < 0) {
            return i;
        }
        wrote += i;
    } while (wrote<len);

    return len;
}

typedef unsigned char byte;

static byte* read_packet_helper(int file_descriptor, bool include_size, uint64_t* size_wb) {
    uint64_t len = 0;
    byte buf[PACKET_SIZE_LEN]; 
    if (read_exact(file_descriptor, buf, PACKET_SIZE_LEN) != PACKET_SIZE_LEN) {
        return NULL;
    }
    len = 0;
    for (int i = (PACKET_SIZE_LEN-1); i >= 0; i--) {
        size_t addad_byte = (size_t)(buf[PACKET_SIZE_LEN - (i+1)]);
        len = len | (addad_byte << (i * 8));
    }
    int extra_for_len = 0;
    if (include_size) {
        extra_for_len = PACKET_SIZE_LEN;
    }
    byte* command_content = erljq_alloc(len + extra_for_len);
    if (command_content == NULL) {
        return NULL;
    }
    if (include_size) {
        for (int i = 0; i < PACKET_SIZE_LEN; i++) {
            command_content[i] = buf[i];
        }
    }
    if (read_exact(file_descriptor, command_content + extra_for_len, len) != len) {
        erljq_free(command_content);
        return NULL;
    }
    if (size_wb != NULL) {
        *size_wb = len + extra_for_len;
    }
    return command_content;
}

static byte* read_packet(int file_descriptor) {
    return read_packet_helper(file_descriptor, false, NULL);
}

static byte* read_whole_packet(int file_descriptor, uint64_t* size_wb) {
    return read_packet_helper(file_descriptor, true, size_wb);
}

static int write_packet(int file_descriptor, byte *buf, size_t len) {
    byte li;

    for (int i = 0; i < PACKET_SIZE_LEN; i++) {
        li = (len >> (8 * (PACKET_SIZE_LEN - (i+1)))) & 0xff;
        write_exact(file_descriptor, &li, 1);
    }

    return write_exact(file_descriptor, buf, len);
}

static bool handle_process_json() {
    byte * jq_program = read_packet(pipe_in);
    if (jq_program == NULL) {
        return false;
    }
    byte * json_data = read_packet(pipe_in);
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
        if (write_packet(STDOUT_FILENO, (byte*)err_tags[JQ_OK], strlen(err_tags[JQ_OK])) <= 0) {
            goto error_on_write_out_0;
        }
        char buf[64];
        sprintf(buf, "%lu", nr_of_result_objects);
        if (write_packet(STDOUT_FILENO, (byte*)buf, strlen(buf)) <= 0) {
            goto error_on_write_out_0;
        }

        for (where = 0; where < nr_of_result_objects; where++) {
            String result = String_dynarr_item_at(&result_strings, where);
            if (write_packet(STDOUT_FILENO, (byte*)result.string, result.size) <= 0) {
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
        // We might have results in the result strings. This can happen
        // when we get a result out from jq before an error happens
        size_t nr_of_result_objects = String_dynarr_size(&result_strings);
        for (size_t i = 0; i < nr_of_result_objects; i++) {
            String result = String_dynarr_item_at(&result_strings, i);
            erljq_free(result.string);
        }
        String_dynarr_destroy(&result_strings);
        if (write_packet(STDOUT_FILENO, (byte*)error_str, strlen(error_str)) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_packet(STDOUT_FILENO, (byte*)err_tags[res], strlen(err_tags[res])) <= 0) {
            goto error_on_write_out_1;
        }
        if (write_packet(STDOUT_FILENO, (byte*)error_msg, strlen(error_msg)) <= 0) {
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
    if (write_packet(STDOUT_FILENO, (byte*)exiting_str, strlen(exiting_str)) <= 0) {
        return false;
    } else {
        fflush(stdout);
        return true;
    }
}

static bool handle_start_record_input() {
    char* recording_file_name = (char*)read_packet(pipe_in);
    if (recording_file_name == NULL) {
        return false;
    }
    record_input_file = fopen(recording_file_name, "wb");
    if (record_input_file == NULL) {
        erljq_free(recording_file_name);
        return false;
    }
    const char* ok_str = "ok";
    if (write_packet(STDOUT_FILENO, (byte*)ok_str, strlen(ok_str)) <= 0) {
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
    if (write_packet(STDOUT_FILENO, (byte*)ok_str, strlen(ok_str)) <= 0) {
        return false;
    }
    record_input = false;
    return true;
}

static bool handle_set_filter_program_lru_cache_size() {
    char* size_str = (char*)read_packet(pipe_in);
    if (size_str == NULL) {
        return false;
    }
    int new_lru_size = atoi(size_str);
    erlang_jq_set_filter_program_lru_cache_size(new_lru_size);
    const char* ok_str = "ok";
    if (write_packet(STDOUT_FILENO, (byte*)ok_str, strlen(ok_str)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_get_filter_program_lru_cache_size() {
    char buffer[64];
    sprintf(buffer, "%d", erlang_jq_get_filter_program_lru_cache_size());
    if (write_packet(STDOUT_FILENO, (byte*)buffer, strlen(buffer)) <= 0) {
        return false;
    }
    return true;
}

static bool handle_ping() {
    const char* response = "pong";
    if (write_packet(STDOUT_FILENO, (byte*)response, strlen(response)) <= 0) {
        return false;
    }
    return true;
}

static void input_forwarder() {
    set_erljq_alloc(jq_port_alloc);
    set_erljq_free(free);
    byte* packet;
    uint64_t size;
    while (true) {
        packet = read_whole_packet(STDIN_FILENO, &size);
        if (packet == NULL) {
            LOG_PRINT("STDIN closed. Erlang VM is dead or port is closed.\n");
            break;
        }
        ssize_t write_exact_res = write_exact(pipe_out, packet, size);
        erljq_free(packet);
        if (write_exact_res != size) {
            LOG_PRINT("Worker thread dead\n");
            break;
        }
    }
    LOG_PRINT("Exit\n");
    // Worker process or input stream is dead or broken: exit
    close(pipe_out);
    close(pipe_in);
    exit(0);
}

void *input_forwarder_thread_start(void *vargp)
{
    input_forwarder();
    return NULL;
}

int main() {
#ifdef ACTIVE_LOG_PRINT
    debug_file = fopen("erlang_jq_port_debug.log", "a");
#endif

    LOG_PRINT("STARTING PORT PROGRAM\n");

    int pipe_ends[2];

    if (pipe(pipe_ends) < 0) {
        LOG_PRINT("Could not open pipe %d\n", errno);
        exit(1);
    }
    pipe_in = pipe_ends[0];
    pipe_out = pipe_ends[1];

    // Creating thread to forward STDIN so we can always detect when STDIN gets closed
    pthread_t thread_id;
    if (pthread_create(&thread_id, NULL, input_forwarder_thread_start, NULL) != 0) {
        LOG_PRINT("Failed to create input forwarder thread\n");
        exit(1);
    }
    LOG_PRINT("In main worker process\n");
    // Start handling input
    erlang_jq_port_process_init();
    while (true) {
        byte* command = read_packet(pipe_in);
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
            // Normal exit communicate back that we are exiting
            return !handle_exit();
        } else if (strcmp((char*)command, "set_filter_program_lru_cache_max_size") == 0) {
            erljq_free(command);
            // Normal exit communicate back that we are exiting
            if (!handle_set_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } else if (strcmp((char*)command, "get_filter_program_lru_cache_max_size") == 0) {
            erljq_free(command);
            // Normal exit communicate back that we are exiting
            if (!handle_get_filter_program_lru_cache_size()) {
                goto error_return;
            }
        } 
        // Recording input is a debugging functionality that can be used to
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
    close(pipe_in);
    close(pipe_out);
    LOG_PRINT("Exiting (the Erlang port may have stopped or crached\n");
    exit(1);
    return 1;
}
