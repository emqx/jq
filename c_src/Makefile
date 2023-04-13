# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>
UNAME_SYS := $(shell uname -s)
ARCH := $(shell uname -m)
CURDIR := .
BASEDIR := $(CURDIR)/..
VERSION := $(shell escript get_version.erl ../src/jq_nif.erl) 
VERSION := $(strip $(VERSION))
JQERLMODSRC := $(BASEDIR)/src/jq_nif.erl

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))
NIF_MODULE := jq_nif
JQURL := https://github.com/emqx/jqc.git
JQSRC_DIR := libs/jqc
JQSRC := $(JQSRC_DIR)/src/jv.c
JQSRC_VERSION := 73c51aa79b67b99c575ef8923aa0c03cd87a7ac1
RDSURL := https://github.com/emqx/c_reusable_data_structures.git
RDSSRC_DIR := libs/c_reusable_data_structures
RDSSRC := $(RDSSRC_DIR)/Makefile
RDSSRC_VERSION := 77c7a96416065a839417e5078fce9ba9a14982dc
LIBJQ_DIR ?= $(JQSRC_DIR)/.libs
LIBJQ_PREFIX := /usr/local
EXT_LIBS := ext_libs
ERL_PORT_PROGRAM := ../priv/erlang_jq_port
ifeq ($(UNAME_SYS), Darwin)
	LIBJQ := libjq.1.dylib
	LIBONIG := libonig.4.dylib
else
	LIBJQ := libjq.so
	LIBONIG := libonig.so
endif

ifdef NORMAL_DEBUG
	EXTRA_C_FLAGS := -fPIC -g -O0
	CC := clang
else ifdef JQ_MEMSAN_DEBUG
	MEMSAN_FLAGS := -fno-omit-frame-pointer -fsanitize=address
	EXTRA_C_FLAGS := -fPIC -g -O0 $(MEMSAN_FLAGS) -I $(ERL_TOP)/erts/emulator/beam/ -I $(ERL_TOP)/erts/include/x86_64-pc-linux-gnu/
	CC := clang
else
	EXTRA_C_FLAGS := -g -fPIC -O3
	MEMSAN_FLAGS := 
endif

LIBJQ_NAME := $(LIBJQ_DIR)/$(LIBJQ)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
JQ_INCLUDE_DIR ?= $(JQSRC_DIR)/src
DS_INCLUDE_DIR ?= libs/c_reusable_data_structures

ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

PRIV_DIR = ../priv
C_SRC_DIR = ./
C_SRC_OUTPUT ?= $(PRIV_DIR)/$(NIF_MODULE)$(VERSION).so

# System type and C compiler/flags.

ifeq ($(UNAME_SYS), Darwin)
	CC ?= gcc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -arch $(ARCH) -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= -O3 -arch $(ARCH) -finline-functions -Wall
	LDFLAGS ?= -arch $(ARCH) -undefined dynamic_lookup
	LD_JQLIBS ?= -ljq -lonig
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= gcc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= $(EXTRA_C_FLAGS) -finline-functions -Wall
	LDFLAGS += -Wl,-rpath,$$ORIGIN
	LD_JQLIBS ?= -l:libjq.a -l:libonig.a
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= $(EXTRA_C_FLAGS) -finline-functions -Wall
	LDFLAGS += -Wl,-rpath,$$ORIGIN
	LD_JQLIBS ?= -l:libjq.a -l:libonig.a
endif

STDC_NO_THREAD := $(shell ./check_if_threads_header_exists.sh "$(CC)" "$(CFLAGS)" threads.h)

NO_PTHREAD := $(shell ./check_if_threads_header_exists.sh "$(CC)" "$(CFLAGS)" pthread.h)

CFLAGS += -pthread -DVERSION=$(VERSION) -D__STDC_NO_THREADS__=$(STDC_NO_THREAD) -DJQ_NO_PTHREAD=$(NO_PTHREAD) -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(JQ_INCLUDE_DIR) -I $(DS_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(JQ_INCLUDE_DIR)
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L $(EXT_LIBS) -lei $(LD_JQLIBS)
LDFLAGS += -pthread $(MEMSAN_FLAGS) -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := erlang_jq_nif.c port_nif_common.c
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

PORT_SOURCES := erlang_jq_port.c port_nif_common.c
PORT_OBJECTS = $(addsuffix .o, $(basename $(PORT_SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

.PHONY: clean check_jqc_version all check_rds_version

all: check_jqc_version check_rds_version $(C_SRC_OUTPUT)

ifeq ($(JQ_MEMSAN_DEBUG), 1)
$(C_SRC_OUTPUT): $(OBJECTS) port_nif_common.h
else
$(C_SRC_OUTPUT): $(RDSSRC) $(OBJECTS) $(ERL_PORT_PROGRAM) port_nif_common.h
endif
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)
ifeq ($(UNAME_SYS), Darwin)
	cp $(JQSRC_DIR)/.libs/$(LIBJQ) $(PRIV_DIR)/
	cp $(JQSRC_DIR)/modules/oniguruma/src/.libs/$(LIBONIG) $(PRIV_DIR)/
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBONIG) "@loader_path/$(LIBONIG)" $(PRIV_DIR)/$(LIBJQ)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBONIG) "@loader_path/$(LIBONIG)" $(PRIV_DIR)/$(LIBJQ)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBONIG) "@loader_path/$(LIBONIG)" $(C_SRC_OUTPUT)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBJQ) "@loader_path/$(LIBJQ)" $(C_SRC_OUTPUT)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBONIG) "@loader_path/$(LIBONIG)" $(ERL_PORT_PROGRAM)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBJQ) "@loader_path/$(LIBJQ)" $(ERL_PORT_PROGRAM)
endif

$(ERL_PORT_PROGRAM): $(PORT_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(CC) $(PORT_OBJECTS) -L $(EXT_LIBS) $(LD_JQLIBS) -lm -pthread -o $(ERL_PORT_PROGRAM)

%.o: %.c $(LIBJQ_NAME) $(JQERLMODSRC) $(RDSSRC) $(PORT_SOURCES)
	$(COMPILE_C) $(OUTPUT_OPTION) $<

check_jqc_version:
	( cd libs/jqc && \
	  [ `git rev-parse --verify HEAD` = '$(JQSRC_VERSION)' ] || (cd .. && rm -rf jqc))


$(JQSRC):
	git clone -b jq-1.6-emqx --single-branch "$(JQURL)" "$(JQSRC_DIR)"
	((cd "$(JQSRC_DIR)" && git checkout $(JQSRC_VERSION)) || \
	     (echo "Failed to check out jq commit" && \
	      rm -r "$(JQSRC_DIR)" && \
	      false))

check_rds_version:
	( cd libs/c_reusable_data_structures && \
	  [ `git rev-parse --verify HEAD` = '$(RDSSRC_VERSION)' ] || (cd .. && rm -rf c_reusable_data_structures))

$(RDSSRC):
	git clone -b master --single-branch "$(RDSURL)" "$(RDSSRC_DIR)"
	((cd "$(RDSSRC_DIR)" && git checkout $(RDSSRC_VERSION)) || \
	     (echo "Failed to check out c_reusable_data_structures commit" && \
	      rm -r "$(RDSSRC_DIR)" && \
	      false))

$(LIBJQ_NAME): $(JQSRC)
	#ls -lart .libs/ modules/oniguruma/src/.libs/
	(cd $(JQSRC_DIR) && \
	git submodule update --init && \
	export CC="$(CC)" && \
	export CFLAGS="$(EXTRA_C_FLAGS)" && \
	export LDFLAGS="$(MEMSAN_FLAGS)" && \
	autoreconf -fi && \
	./configure --with-oniguruma=builtin --disable-maintainer-mode --prefix=$(LIBJQ_PREFIX) && \
	$(MAKE) -C modules/oniguruma/ && \
	$(MAKE) src/builtin.inc && \
	$(MAKE) libjq.la) && \
	(mkdir $(EXT_LIBS) || true) && \
	cp $(JQSRC_DIR)/.libs/libjq.* $(EXT_LIBS)/ && \
	cp $(JQSRC_DIR)/modules/oniguruma/src/.libs/libonig.* $(EXT_LIBS)/
clean:
	@rm -f $(OBJECTS)
	@rm -f $(PORT_OBJECTS)
	@rm -f $(PRIV_DIR)/*
	@rm -rf $(EXT_LIBS)
	@rm -rf $(JQSRC_DIR)
	@rm -rf $(RDSSRC_DIR)
	@rm -f $(ERL_PORT_PROGRAM)

