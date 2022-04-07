#!/bin/sh

grep '.*define.*(.*VERSION' "$1"/src/jq.erl | sed -E 's/.*VERSION.*,.*([[:digit:]]+).*/\1/'

