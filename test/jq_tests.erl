-module(jq_tests).
-include_lib("eunit/include/eunit.hrl").

process_error_test_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    ].

object_identifier_index_test_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\":\n 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].

array_index_test_() ->
    [ ?_assertEqual({ok,[<<"1">>,<<"2">>,<<"3">>]}, jq:parse(<<".b|.[]">>, <<"{\"b\": [1,2,3]}">>))
    ].
