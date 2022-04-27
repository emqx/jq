#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "$SCRIPT_DIR/.."

if [ ! -d otp ]
then
    git clone git@github.com:erlang/otp.git

    cd otp

    export ERL_TOP=`pwd`
    export PATH=$ERL_TOP/bin:$PATH
    
    # Checking out OTP-24.2.1
    git checkout 7bf7f01683acf9b8f09bd8c7331854a9abc17f7d
    if [ $? != 0 ]
    then
        echo "Could not check out desired Erlang version"
        exit 1
    fi

    ./configure

    make -j 4

    cd erts/emulator

    make asan
fi

cd "$SCRIPT_DIR/../otp"

export ERL_TOP=`pwd`
export PATH=$ERL_TOP/bin:$PATH

cd "$SCRIPT_DIR/.."

(rm -rf c_src/libs/jqc || true)
(rm -rf c_src/ext_libs || true)
rebar3 clean

(mkdir c_src/ext_libs || true)

cp "$ERL_TOP/lib/erl_interface/obj/x86_64-pc-linux-gnu/libei.a" c_src/ext_libs/

export JQ_MEMSAN_DEBUG=1

rebar3 as addr_san_test,test compile

echo "======================================================================================="
echo "Things are now set up to run tests with address sanitizer (if nothing has gone wrong)"
echo
echo "The following have been compiled with address sanitizer :"
echo
echo "1. An Erlang VM (located in jq/otp)"
echo "2. The jq library"
echo "3. The jq NIF library"
echo
echo "You can now run the eunit test with address sanitizer  with the command:"
echo "./test/address_sanitizer_run_eunit.sh"
echo
echo "To run or compile the jq NIF library without address sanitizer  again, you"
echo "have to first run \"rebar3 clean\""
echo "======================================================================================="
