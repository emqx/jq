-module(jqerl_tests).
-include_lib("eunit/include/eunit.hrl").

object_identifier_index_test_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jqerl:parse(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jqerl:parse(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jqerl:parse(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].

