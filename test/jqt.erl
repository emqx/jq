-module(jqt).

-export([run1/1, run2/1, run3/1, run4/1, run5/1, run_no_data/1, foo1/1, leak/0]).

%%Prog = <<"if (.properties.keys | index(\"name\")) != null then .properties.values[ (.properties.keys | index(\"name\")) ].string_value else \"\" end">>,
%%Data = <<"{\"string_value\":\"instance-123\",\"properties\":{\"values\":[{\"string_value\":\"Temperature Sensor\"},{\"string_value\":\"Monitors motor heat\"},{\"string_value\":\"modbus-tcp\"}],\"keys\":[\"name\",\"description\",\"protocol_id\"]},\"name\":\"Instance Id\",\"alias\":1}">>,
%%
run1(N) ->
    Prog = <<".keys | index(\"name\")">>,
    Data = <<"{\"keys\":[\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"]}">>,
    run_prog_data(Prog, Data, N).

run2(N) ->
    Prog = <<".keys | index(\"name\")">>,
    Data = <<"{\"keys\":[\"yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy\"]}">>,
    run_prog_data(Prog, Data, N).

run3(N) ->
    Prog = <<".keys | index(\"name\")">>,
    Data = <<"{\"keys\":[\"name\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"9\",\"0\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"9\"]}">>,
    run_prog_data(Prog, Data, N).

run4(N) ->
    Prog = <<".keys | to_entries | map(select(.value == \"name\")) | .[0].key">>,
    Data = <<"{\"keys\":[\"0\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"9\",\"0\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"name\"]}">>,
    run_prog_data(Prog, Data, N).

run5(N) ->
    Prog = <<".keys as $ks | range(0; $ks|length) | select($ks[.] == \"name\")">>,
    Data = <<"{\"keys\":[\"0\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"9\",\"0\", \"1\",\"2\",\"3\",\"4\",\"5\", \"6\", \"7\", \"8\", \"name\"]}">>,
    run_prog_data(Prog, Data, N).

run_prog_data(_Prog, _Data, 0) ->
    ok;
run_prog_data(Prog, Data, N) ->
    _ = jq:process_json(Prog, Data),
    run_prog_data(Prog, Data, N - 1).

run_no_data(0) ->
    ok;
run_no_data(N) ->
    Prog = <<"if (.properties.keys | index(\"name\")) != null then .properties.values[ (.properties.keys | index(\"name\")) ].string_value else \"\" end">>,
    Data = <<"{}">>,
    _ = jq:process_json(Prog, Data),
    run_no_data(N - 1).

foo1(0) ->
    ok;
foo1(N) ->
    Prog = <<".foo">>,
    Data = <<"{\"foo\":1}">>,
    _ = jq:process_json(Prog, Data),
    foo1(N - 1).

leak() ->
    Pid = os:getpid(),
    Ps = list_to_integer(string:strip(os:cmd("ps -o rss= -p " ++ Pid),right,$\n)) * 1024,
    Erl = erlang:memory(total),
    round((Ps - Erl) / 1024 / 1024).
