#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )


TOP_DIR=`git rev-parse --show-toplevel`

cd "$SCRIPT_DIR/.."

valgrind --version > /dev/null

if [ $? != 0 ]
then
    echo SKIP
    exit 1
fi

# Generate port program input
erl -noshell -pa "$TOP_DIR/_build/test/lib/jq/ebin" -pa "$TOP_DIR/_build/test/lib/jq/test" -eval "jq_tests:generate_port_program_input(\"$SCRIPT_DIR/test_record.bin\"),erlang:halt()"

# Run port program under valgrind with the generated input

cat "$SCRIPT_DIR/test_record.bin" | valgrind --leak-check=full ./priv/erlang_jq_port > /dev/null 1>"$SCRIPT_DIR/port_program_stdout.bin" 2> "$SCRIPT_DIR/port_program_stderr.txt"

cat "$SCRIPT_DIR/port_program_stderr.txt" | grep 'ERROR SUMMARY: 0 error' > /dev/null

if [ $? != 0 ]
then
    echo ERROR
    exit 1
fi

echo SUCCESS
