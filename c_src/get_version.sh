#!/bin/sh


grep '.*define.*(.*VERSION' "$1"/src/jq.erl | sed -e 's/.*VERSION.*,.*\([[:digit:]]\+\).*/\1/'
