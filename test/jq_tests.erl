-module(jq_tests).

-export([generate_port_program_input/1]).

-include_lib("eunit/include/eunit.hrl").

 %% -define(TEST_ONLY_NIF, 1).


-ifdef(TEST_ONLY_NIF).
wrap_setup_cleanup(TestCases) ->
    [
     {setup,
     fun setup_nif/0,
     fun cleanup_nif/1,
     TestCases}
    ].
-else.
wrap_setup_cleanup(TestCases) ->
    [
     {setup,
      fun setup_nif/0,
      fun cleanup_nif/1,
      TestCases}
     ,
     {setup,
      fun setup_port/0,
      fun cleanup_port/1,
      TestCases}
    ].
-endif.

change_get_cache_size_t() ->
    [ ?_assertMatch(ok, jq:set_filter_program_lru_cache_max_size(42)),
      ?_assertMatch(42, jq:get_filter_program_lru_cache_max_size())
    ].
change_get_cache_size_test_() -> wrap_setup_cleanup(change_get_cache_size_t()).

empty_input_t_() ->
    [
       ?_assertMatch({error, {jq_err_parse, _}}, jq:process_json(<<".">>, <<"">>))
     , ?_assertMatch({error, {jq_err_parse, _}}, jq:process_json(<<".">>, <<" ">>))
     , ?_assertMatch({ok,[<<"{}">>]}, jq:process_json(<<"">>, <<"{}">>))
     , ?_assertMatch({ok,[<<"{}">>]}, jq:process_json(<<" ">>, <<"{}">>))].
empty_input_test_() -> wrap_setup_cleanup(empty_input_t_()).

include_t_() ->
    [
       ?_assertMatch({error, {jq_err_compile, _}}, jq:process_json(<<"include \"i_do_not_exist\";.">>, <<"1">>))
    ].
include_test_() -> wrap_setup_cleanup(include_t_()).

large_program_t_() ->
    Program = (
        "def rem_first:\n"
        "    if length > 2 then del(.[0]) else . end;\n"
        "def rem_last:\n"
        "    if length > 1 then del(.[-1]) else . end;\n"
        ".date as $date |\n"
        ".name as $name |\n"
        ".sensors[] |\n"
        "(.data | sort | rem_first | rem_last | add / length) as $average |\n"
        "{name: $name, average: $average, date: $date}"),
    Data = (
        "{"
        "  \"date\": \"2020-04-24\","
        "  \"sensors\": ["
        "    {"
        "      \"name\": \"a\","
        "      \"data\": [-1,2,3,4,5]"
        "    },"
        "    {"
        "      \"name\": \"b\","
        "      \"data\": [1,-100,2,3,4,5,2000]"
        "    },"
        "    {"
        "      \"name\": \"c\","
        "      \"data\": [7]"
        "    }"
        "  ]"
        "}"),
    [
       ?_assertMatch({ok,[<<"{\"name\":null,\"average\":3,\"date\":\"2020-04-24\"}">>,
                          <<"{\"name\":null,\"average\":3,\"date\":\"2020-04-24\"}">>,
                          <<"{\"name\":null,\"average\":7,\"date\":\"2020-04-24\"}">>]},
                     jq:process_json(Program, Data))
    ].
large_program_test_() -> wrap_setup_cleanup(large_program_t_()).

large_result_input_t_() ->
    Data = erlang:iolist_to_binary(io_lib:format("~w", [lists:seq(1, 10000)])),
    [
       ?_assertMatch({ok, [Data]}, jq:process_json(".", Data))
    ].
large_result_input_test_() -> wrap_setup_cleanup(large_result_input_t_()).

timeout_bad_arg_t_() ->
    [?_assertMatch({error, _}, jq:process_json(".", "42", -1)),
     ?_assertMatch({error, _}, jq:process_json(".", "42", hej))].
timeout_bad_arg_test_() -> wrap_setup_cleanup(timeout_bad_arg_t_()).

timeout_t_() ->
    Program = (
        "def while(cond; update):"
        "  def _while:"
        "    if cond then  (update | _while) else . end;"
        "  _while;"
        "while(. < 42; . * 2)"),
    TO = fun() -> {error, {timeout, _}} = jq:process_json(Program, "-2", 10) end,
    OK = fun() -> {ok, [<<"64">>]} = jq:process_json(Program, "2", 10000) end,
    NrOfSubProcesses = 30,
    TimeoutAndThenNot = 
    fun () ->
        OK(),
        TO(),
        OK(),
        Parent = self(),
        ct:pal("Starting concurrent processes that will timeout. Implementation module ~p", [jq:implementation_module()]),
        Pids = [ spawn_link(fun() ->
                                    % ct:pal("Starting process ~p ~p", [X, self()]),
                                    OK(),
                                    TO(),
                                    OK(),
                                    Parent ! self() end) ||
                 X <- lists:seq(1, NrOfSubProcesses)],
        ct:pal("Starting to wait for processes"),
        Sum = lists:sum([ begin 
                              % erlang:display({'Process finnished timeout', X}),
                              receive X -> 1 end
                          end || X <- Pids]),
        ct:pal("Done with concurrent timeout test"),
        Sum
    end,
    %% Timout setting does not seem to be working on Mac OS (a bug?)
    {timeout, 3000, [
       ?_assertMatch({ok, [<<"64">>]},
                     jq:process_json(Program, "2", 10000))
     , ?_assertMatch({ok, [<<"64">>]},
                     jq:process_json(Program, "2", infinity))
     , ?_assertMatch({ok, [<<"64">>]},
                     jq:process_json(Program, "2"))
     , ?_assertMatch({error, {timeout, _}},
                     jq:process_json(Program, "-2",100))
     , ?_assertMatch(NrOfSubProcesses, TimeoutAndThenNot())
    ]}.
timeout_test_() -> wrap_setup_cleanup(timeout_t_()).

parse_error_t_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:process_json(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:process_json(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:process_json(<<".">>, <<"{\"b\"- 2}">>))
    ].
parse_error_test_() -> wrap_setup_cleanup(parse_error_t_()).

process_error_t_() ->
    [ ?_assertMatch({error, {jq_err_process, _}}, jq:process_json(<<".[1]">>, <<"{}">>))
    , ?_assertMatch({error, {jq_err_process, _}}, jq:process_json(<<".a">>, <<"[1,2]">>))
    ].
process_error_test_() -> wrap_setup_cleanup(process_error_t_()).

object_identifier_index_t_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:process_json(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:process_json(<<".">>, <<"{\"b\":\n 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:process_json(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:process_json(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].
object_identifier_index_test_() -> wrap_setup_cleanup(object_identifier_index_t_()).

array_index_t_() ->
    [ ?_assertEqual({ok,[<<"1">>,<<"2">>,<<"3">>]}, jq:process_json(<<".b|.[]">>, <<"{\"b\": [1,2,3]}">>))
    ].
array_index_test_() -> wrap_setup_cleanup(array_index_t_()).

test_prog(ExpectedResStr, FilterProgStr, InputStr) ->
    ExpectedResBin = erlang:list_to_binary(ExpectedResStr),
    FilterProgBin = erlang:list_to_binary(FilterProgStr),
    InputBin = erlang:list_to_binary(InputStr),
    ?_assertEqual({ok, [ExpectedResBin]},
                  jq:process_json(
                    FilterProgBin,
                    InputBin)).

advanced_filter_programs_t() ->
    %% Programs here taken from https://jqplay.org/
    [
     test_prog(
       "\"many\"",
       "if . == 0 then \"zero\" elif . == 1 then \"one\" else \"many\" end",
       "2"),
     test_prog(
       "\"The input was 42, which is one less than 43\"",
       "\"The input was \\(.), which is one less than \\(.+1)\"",
       "42"),
     test_prog(
       "\"The input was 42, which is one less than 43\"",
       "\"The input was \\(.), which is one less than \\(.+1)\"",
       "42"),
     test_prog(
       "[2,3,4]",
       "map(.+1)",
       "[1,2,3]"),
     test_prog(
       "[5,3,7]",
       "map(select(. >= 2))",
       "[1,5,3,0,7]"),
     test_prog(
       "[\"JSON\",\"XML\"]",
       "[.[] | .name]",
       "[{\"name\":\"JSON\", \"good\":true}, {\"name\":\"XML\", \"good\":false}]"),
     test_prog(
       "[42,\"something else\"]",
       "[.foo, .bar]",
       "{ \"foo\": 42, \"bar\": \"something else\", \"baz\": true}"),
     test_prog(
       "[\"Foo\",\"abc\",\"abcd\"]",
       "keys",
       "{\"abc\": 1, \"abcd\": 2, \"Foo\": 3}"),
     test_prog(
       "[2,6,1,0]",
       "[.[] | length]",
       "[[1,2], \"string\", {\"a\":2}, null]"),
     test_prog(
       "[{\"user\":\"stedolan\",\"title\":\"JQ Primer\"},{\"user\":\"stedolan\",\"title\":\"More JQ\"}]",
       "[{user, title: .titles[]}]",
       "{\"user\":\"stedolan\",\"titles\":[\"JQ Primer\", \"More JQ\"]}"),
     test_prog(
       "42",
       ".foo",
       "{\"foo\": 42, \"bar\": \"less interesting data\"}"),
     test_prog(
       "{\"name\":\"XML\",\"good\":false}",
       ".[1]",
       "[{\"name\":\"JSON\", \"good\":true}, {\"name\":\"XML\", \"good\":false}]")
    ].
advanced_filter_programs_test_() ->
    wrap_setup_cleanup(advanced_filter_programs_t()).

get_tests_cases() ->
    [ erlang:element(2, Test) ||
      Test <-
      lists:flatten([empty_input_t_(),
                     include_t_(),
                     parse_error_t_(),
                     process_error_t_(),
                     object_identifier_index_t_(),
                     array_index_t_(),
                     advanced_filter_programs_t()
                    ])
    ].

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
                            CacheSize});
        false -> ok
    end,
    ok = jq:set_filter_program_lru_cache_max_size(OldCacheSize),
    ok.

qubes_helper(0, SoFar) ->
    SoFar;
qubes_helper(N, SoFar) ->
    qubes_helper(N div 2, [N | SoFar]).
qubes(N) ->
    qubes_helper(N, []).

concurrent_queries_t_() ->
    {timeout, erlang:system_info(schedulers) * 14,
     fun() ->
             NrOfScheds = erlang:system_info(schedulers),
             Qubes = qubes(NrOfScheds),
             erlang:display_nl(),
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 500, 5000))
              || NrOfTestProcess <- Qubes],
             ok = concurrent_queries_test(NrOfScheds, false, 0, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 1, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 3, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 10, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 2, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 100, 150),
             ok
     end}.
concurrent_queries_test_() -> wrap_setup_cleanup(concurrent_queries_t_()).

-ifndef(TEST_ONLY_NIF).
port_program_valgrind_test_() ->
    {timeout, 30,
     fun() ->
             jq:set_implementation_module(jq_port),
             TestDir = filename:dirname(code:which(?MODULE)),
             TestScript = filename:join(TestDir, "valgrind_port_program.sh"),
             Result = os:cmd(TestScript),
             case re:run(Result, "SUCCESS|SKIP") =/= nomatch of
                 true -> ok;
                 false ->
                     ErrorMsg = io_lib:format("\nMemory Error. See: ~s",
                                              [filename:join(TestDir, "port_program_stderr.txt\n")]),
                     erlang:display_string(erlang:binary_to_list(erlang:iolist_to_binary(ErrorMsg))),
                     error("Valgrind reported error(s)")
             end
     end}.
-endif.

setup_nif() ->
    ct:pal("Setup NIF, Schedulers online ~p", [erlang:system_info(schedulers_online)]),
    PrevImpMod = jq:implementation_module(),
    jq:set_implementation_module(jq_nif),
    PrevImpMod.

%% This function attempts to trigger global gc by adding and removing terms
%% from persistent storage
trigger_global_gc() ->
    Cnt = 20,
    timer:sleep(200),
    [persistent_term:put({waste_term, X}, lists:seq(1, 100 + X)) || X <- lists:seq(1, Cnt)],
    [persistent_term:erase({waste_term, X}) || X <- lists:seq(1, Cnt)].


cleanup_nif(PrevImpMod) ->
    ct:pal("Cleanup NIF ~p", [erlang:system_info(schedulers_online)]),
    %% For some reason this seems to clean up NIF resources that are previously
    %% passed to timer:apply_after in the parameters list
    trigger_global_gc(),
    OuterSelf = self(),
    TimerFun =
       fun() ->
               Self = self(),
               timer:apply_after(10, erlang, send, [Self, timeout_done]),
               receive timeout_done -> ok end,
               OuterSelf ! fixed
       end,
    NrOfSpawns = 150,
    [spawn(TimerFun) || _ <- lists:seq(1, NrOfSpawns)],
    [receive fixed -> ok end || _ <- lists:seq(1, NrOfSpawns)],
    trigger_global_gc(),
    true = code:delete(jq_nif),
    true = code:soft_purge(jq_nif),
    jq:set_implementation_module(PrevImpMod).

setup_port() ->
    ct:pal("Setup Port, Schedulers online ~p", [erlang:system_info(schedulers_online)]),
    PrevImpMod = jq:implementation_module(),
    jq:set_implementation_module(jq_port),
    PrevImpMod.

cleanup_port(PrevImpMod) ->
    ct:pal("Cleanup Port"),
    application:stop(jq),
    jq:set_implementation_module(PrevImpMod).

generate_port_program_input(RecordFilePath) ->
    Schedulers = erlang:system_info(schedulers_online),
    erlang:system_flag(schedulers_online, 1),
    jq_port:start_recording(RecordFilePath),
    ok = concurrent_queries_test(1, false, 100, 60),
    ok = concurrent_queries_test(1, false, 1, 10),
    ok = concurrent_queries_test(1, false, 3, 10),
    ok = concurrent_queries_test(1, false, 0, 10),
    application:stop(jq),
    erlang:system_flag(schedulers_online, Schedulers),
    ok.
