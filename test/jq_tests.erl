-module(jq_tests).
-include_lib("eunit/include/eunit.hrl").


empty_input_test_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<" ">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<"">>, <<"{}">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<" ">>, <<"{}">>))
    ].

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

repeat_tests(Parent, ShouldStop, [], AllTestFuns, Cnt) ->
    case counters:get(ShouldStop, 1) of
        0 ->
            repeat_tests(Parent, ShouldStop, AllTestFuns, AllTestFuns, Cnt);
        _ -> 
            Parent ! {test_process_stopped, Cnt}
    end;
repeat_tests(Parent, ShouldStop, [TestFun | RemTestFuns], AllTestFuns, Cnt) ->
    TestFun(),
    repeat_tests(Parent, ShouldStop, RemTestFuns, AllTestFuns, Cnt + 1).

concurrent_queries_test(NrOfTestProcess) ->
    TestTimeMs = 1000,
    ShouldStop = counters:new(1, []),
    TestCases = get_tests_cases(), 
    Self = erlang:self(),
    TestRunner = fun() ->
                    repeat_tests(Self, ShouldStop, TestCases, TestCases, 0)
                 end,
    [erlang:spawn_link(TestRunner) || _ <- lists:seq(1, NrOfTestProcess)], 
    timer:sleep(TestTimeMs),
    counters:add(ShouldStop, 1, 1),
    Cnts = [receive {test_process_stopped, Cnt} -> Cnt end || _ <- lists:seq(1, NrOfTestProcess)], 
    erlang:display({'# of processes', NrOfTestProcess, 'Test Cases / Second', lists:sum(Cnts)}),
    ok.

concurrent_queries_test_() ->
    {timeout, erlang:system_info(schedulers) * 2,
     fun() ->
             erlang:display_nl(),
             [(ok = concurrent_queries_test(NrOfTestProcess))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             ok
     end}.
