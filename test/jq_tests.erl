-module(jq_tests).
-include_lib("eunit/include/eunit.hrl").

parse_error_test_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    ].

process_error_test_() ->
    [ ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".[1]">>, <<"{}">>))
    , ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".a">>, <<"[1,2]">>))
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

get_tests_cases() ->
    [ erlang:element(2, Test) ||
      Test <-
      lists:flatten([ 
                     parse_error_test_(),
                     process_error_test_(),
                     object_identifier_index_test_(),
                     array_index_test_()])].

repeat_tests(Parent, ShouldStop, [], AllTestFuns) ->
    case counters:get(ShouldStop, 1) of
        0 ->
            repeat_tests(Parent, ShouldStop, AllTestFuns, AllTestFuns);
        _ -> 
            Parent ! test_process_stopped
    end;
repeat_tests(Parent, ShouldStop, [TestFun | RemTestFuns], AllTestFuns) ->
    TestFun(),
    repeat_tests(Parent, ShouldStop, RemTestFuns, AllTestFuns).

concurrent_queries_test() ->
    NrOfTestProcess = 4,
    TestTimeMs = 1000,
    ShouldStop = counters:new(1, []),
    TestCases = get_tests_cases(), 
    Self = erlang:self(),
    TestRunner = fun() ->
                    repeat_tests(Self, ShouldStop, TestCases, TestCases)
                 end,
    [erlang:spawn_link(TestRunner) || _ <- lists:seq(1, NrOfTestProcess)], 
    timer:sleep(TestTimeMs),
    counters:add(ShouldStop, 1, 1),
    [receive test_process_stopped -> ok end || _ <- lists:seq(1, NrOfTestProcess)], 
    ok.
