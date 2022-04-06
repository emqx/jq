-module(jq_tests).

-include_lib("eunit/include/eunit.hrl").

wrap_setup_cleanup(TestCases) ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     TestCases}.

change_get_cache_size_t() ->
    [ ?_assertMatch(ok, jq:set_filter_program_lru_cache_max_size(42)),
      ?_assertMatch(42, jq:get_filter_program_lru_cache_max_size())
    ].
change_get_cache_size_test_() -> wrap_setup_cleanup(change_get_cache_size_t()).

empty_input_t_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<" ">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<"">>, <<"{}">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<" ">>, <<"{}">>))
    ].
empty_input_test_() -> wrap_setup_cleanup(empty_input_t_()).

parse_error_t_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    ].
parse_error_test_() -> wrap_setup_cleanup(parse_error_t_()).

process_error_t_() ->
    [ ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".[1]">>, <<"{}">>))
    , ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".a">>, <<"[1,2]">>))
    ].
process_error_test_() -> wrap_setup_cleanup(process_error_t_()).

object_identifier_index_t_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\":\n 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].
object_identifier_index_test_() -> wrap_setup_cleanup(object_identifier_index_t_()).

array_index_t_() ->
    [ ?_assertEqual({ok,[<<"1">>,<<"2">>,<<"3">>]}, jq:parse(<<".b|.[]">>, <<"{\"b\": [1,2,3]}">>))
    ].
array_index_test_() -> wrap_setup_cleanup(array_index_t_()).

get_tests_cases() ->
    [ erlang:element(2, Test) ||
      Test <-
      lists:flatten([empty_input_t_(), 
                     parse_error_t_(),
                     process_error_t_(),
                     object_identifier_index_t_(),
                     array_index_t_()])].

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

concurrent_queries_test(NrOfTestProcesses, PrintThroughput, CacheSize, TestTimeMs) ->
    ShouldStop = counters:new(1, []),
    TestCases = get_tests_cases(), 
    Self = erlang:self(),
    OldCacheSize = jq:get_filter_program_lru_cache_max_size(),
    ok = jq:set_filter_program_lru_cache_max_size(CacheSize),
    TestRunner = fun() ->
                    repeat_tests(Self, ShouldStop, TestCases, TestCases, 0)
                 end,
    [erlang:spawn_link(TestRunner) || _ <- lists:seq(1, NrOfTestProcesses)], 
    timer:sleep(TestTimeMs),
    counters:add(ShouldStop, 1, 1),
    Cnts = [receive {test_process_stopped, Cnt} -> Cnt end || _ <- lists:seq(1, NrOfTestProcesses)], 
    case PrintThroughput of
        true ->
            Throughput = erlang:floor(lists:sum(Cnts) / (TestTimeMs / 1000)),
            erlang:display({'# of processes',
                            NrOfTestProcesses,
                            'Test Cases / Second',
                            Throughput,
                            'cache size',
                            CacheSize})
    end,
    ok = jq:set_filter_program_lru_cache_max_size(OldCacheSize),
    ok.

concurrent_queries_t_() ->
    {timeout, erlang:system_info(schedulers) * 3,
     fun() ->
             erlang:display_nl(),
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 500, 1000))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 0, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 1, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 3, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 5, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 10, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 2, 100))
              || NrOfTestProcess <- lists:seq(1, erlang:system_info(schedulers))],
             ok
     end}.
concurrent_queries_test_() -> wrap_setup_cleanup(concurrent_queries_t_()).

setup() ->
    ok.

cleanup(_) ->
    true = code:delete(jq),
    true = code:soft_purge(jq).
