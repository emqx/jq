#!/usr/bin/env bash

echo "============================================"
echo "Note that the \"test/address_sanitizer_setup.sh\" has to run"
echo "successfully at least once before this script will work."
echo "============================================"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$SCRIPT_DIR/../otp"

export ERL_TOP=`pwd`
export PATH=$ERL_TOP/bin:$PATH
    
cd "$SCRIPT_DIR/.."
export JQ_MEMSAN_DEBUG=1
#rebar3 as addr_san_test eunit
rebar3 eunit
echo "The asan error above can most likely be ignored if everything below runs without problems"

echo "IMPORTANT!!!!"
echo "The following command does not work as expected: rebar3 as addr_san_test eunit (maybe the profile does not have any effect on the test?)"
echo "You therefore need to uncomment -define(TEST_ONLY_NIF, 1). in jq_tests.erl before running this script!"
echo "IMPORTANT!!!!"
echo "============================================"
echo "Running the eunit test with address sanitizer"
echo "============================================"

export ASAN_LOG_DIR=`pwd`/asan_logs

(rm -rf "$ASAN_LOG_DIR" || true)

mkdir "$ASAN_LOG_DIR"

# ASAN_OPTIONS=intercept_tls_get_addr=0 is a workaround for a bug in address sanitizer that we hit
# https://github.com/google/sanitizers/issues/1322
# https://github.com/neovim/neovim/pull/17213

ASAN_OPTIONS=intercept_tls_get_addr=0 cerl -asan -pa _build/test/lib/jq/test/ -pa _build/test/lib/jq/ebin/ -eval "jq_tests:test(),erlang:halt()" 

echo "============================================"
echo "The address sanitizer log (located in \"$ASAN_LOG_DIR\") will now be printed:"
echo "============================================"

cat "$ASAN_LOG_DIR/`ls "$ASAN_LOG_DIR"`"

