#!/bin/sh
TEST_FILE=test_if_threads_header_exists.c
echo "#include <$3>" > $TEST_FILE
echo 'int main(){return 0;}' >> $TEST_FILE
$1 $2 $TEST_FILE 2>/dev/null
(test $? = 0 && echo 0) || echo 1
rm $TEST_FILE
