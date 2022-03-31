# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>
UNAME_SYS := $(shell uname -s)
CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))
JQURL := https://github.com/terry-xiaoyu/jqc.git
JQSRC_DIR := $(CURDIR)/libs/jqc
JQSRC := $(JQSRC_DIR)/src/jv.c
RDSURL := https://github.com/kjellwinblad/c_reusable_data_structures.git
RDSSRC_DIR := $(CURDIR)/libs/c_reusable_data_structures
RDSSRC := $(RDSSRC_DIR)/Makefile
LIBJQ_DIR ?= $(JQSRC_DIR)/.libs
LIBJQ_PREFIX := /usr/local
EXT_LIBS := $(CURDIR)/ext_libs

ifeq ($(UNAME_SYS), Darwin)
	LIBJQ := libjq.1.dylib
	LIBONIG := libonig.4.dylib
else
	LIBJQ := libjq.so
	LIBONIG := libonig.so
endif

ifdef JQ_MEMSAN_DEBUG
	MEMSAN_FLAGS := -fno-omit-frame-pointer -fsanitize=address
	EXTRA_C_FLAGS := -fPIC -g -O0 $(MEMSAN_FLAGS) -I $(ERL_TOP)/erts/emulator/beam/ -I $(ERL_TOP)/erts/include/x86_64-pc-linux-gnu/
	CC := clang
else
	EXTRA_C_FLAGS := -fPIC -O3
	MEMSAN_FLAGS := 
endif

LIBJQ_NAME := $(LIBJQ_DIR)/$(LIBJQ)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
JQ_INCLUDE_DIR ?= $(JQSRC_DIR)/src
DS_INCLUDE_DIR ?= libs/c_reusable_data_structures

ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

PRIV_DIR = $(CURDIR)/../priv
C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(PRIV_DIR)/$(PROJECT).so

# System type and C compiler/flags.

ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -arch x86_64 -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= $(EXTRA_C_FLAGS) -finline-functions -Wall
	LDFLAGS += -Wl,-rpath,$$ORIGIN
else ifeq ($(UNAME_SYS), Linux)
	CC ?= cc
	CFLAGS ?= $(EXTRA_C_FLAGS) -std=c11 -finline-functions -Wall -Wno-missing-prototypes -Wno-unused-function
	CXXFLAGS ?= $(EXTRA_C_FLAGS) -finline-functions -Wall
	LDFLAGS += -Wl,-rpath,$$ORIGIN
endif

CFLAGS += -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(JQ_INCLUDE_DIR) -I $(DS_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(JQ_INCLUDE_DIR)
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L $(EXT_LIBS) -lei -l:libjq.a -l:libonig.a
LDFLAGS += $(MEMSAN_FLAGS) -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -maxdepth 1 -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

.PHONY: clean build_erl_jq

ifeq ($(UNAME_SYS), Darwin)
$(C_SRC_OUTPUT): build_erl_jq
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBONIG) "@loader_path/$(LIBONIG)" $(PRIV_DIR)/$(LIBJQ)
	install_name_tool -change $(LIBJQ_PREFIX)/lib/$(LIBJQ) "@loader_path/$(LIBJQ)" $(C_SRC_OUTPUT)
else
$(C_SRC_OUTPUT): build_erl_jq
endif

build_erl_jq: $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(C_SRC_OUTPUT)

%.o: %.c $(LIBJQ_NAME)
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc $(LIBJQ_NAME)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C $(LIBJQ_NAME)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp $(LIBJQ_NAME)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

$(JQSRC):
	git clone -b jq-1.6-emqx --single-branch $(JQURL) $(JQSRC_DIR)
	((cd $(JQSRC_DIR) && git checkout 51df83ccf4808867eee79a011c71a93a121be1b6) || \
	     (echo "Failed to check out jq commit" && \
	      rm -r $(JQSRC_DIR) && \
	      false))

$(RDSSRC):
	git clone -b master --single-branch $(RDSURL) $(RDSSRC_DIR)
	((cd $(RDSSRC_DIR) && git checkout 40cdd2544cfbd697ec9fa6d70fd971560cd9b4ff) || \
	     (echo "Failed to check out c_reusable_data_structures commit" && \
	      rm -r $(RDSSRC_DIR) && \
	      false))

$(LIBJQ_NAME): $(JQSRC) $(RDSSRC)
	#ls -lart .libs/ modules/oniguruma/src/.libs/
	cd $(JQSRC_DIR) && \
	git submodule update --init && \
	export CC="$(CC)" && \
	export CFLAGS="$(EXTRA_C_FLAGS)" && \
	export LDFLAGS="$(MEMSAN_FLAGS)" && \
	autoreconf -fi && \
	./configure --with-oniguruma=builtin --prefix=$(LIBJQ_PREFIX) && \
	make -C modules/oniguruma/ && \
	make src/builtin.inc && \
	make libjq.la && \
	cp .libs/$(LIBJQ) $(PRIV_DIR)/ && \
	cp modules/oniguruma/src/.libs/$(LIBONIG) $(PRIV_DIR)/ && \
	(mkdir $(EXT_LIBS) || true) && \
	cp .libs/libjq.* $(EXT_LIBS)/ && \
	cp modules/oniguruma/src/.libs/libonig.* $(EXT_LIBS)/ && \
	cd $(C_SRC_DIR)

clean:
	@rm -f $(OBJECTS)
	@rm -f $(PRIV_DIR)/*
	@rm -rf $(EXT_LIBS)
	@rm -rf $(JQSRC_DIR)/libjq.la $(LIBJQ_DIR)

