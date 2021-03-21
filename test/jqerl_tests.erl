-module(jqerl_tests).
-include_lib("eunit/include/eunit.hrl").

process_error_test_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jqerl:parse(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jqerl:parse(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jqerl:parse(<<".">>, <<"{\"b\"- 2}">>))
    ].

object_identifier_index_test_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jqerl:parse(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jqerl:parse(<<".">>, <<"{\"b\":\n 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jqerl:parse(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jqerl:parse(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].
