
%%%-------------------------------------------------------------------
%% @doc gen_server callback module implementing a server controlling a jq port
%% program
%% @end
%%%-------------------------------------------------------------------


-module(jq_port).

-behavior(gen_server).

%% Gen server calls
-export([
          handle_call/3
        , handle_cast/2
        , init/1
        , terminate/2
        , handle_info/2
        , code_change/3
        ]).

%% For supervisor

-export([start_link/1]).

%% Public interface

-export([process_json/2, process_json/3]).

%% Configuration functions
-export([
           set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0
         , nr_of_jq_port_servers/0
         , set_nr_of_jq_port_servers/1
        ]).

%% Debug functionality

-export([
           start_recording/1
         , stop_recording/0
        ]).

-define(APPNAME, jq).

start_link(Id) ->
    gen_server:start_link(jq_port, Id, []).

do_op_ensure_started_helper(_Op, 0) ->
    error("Could not start jq port servers");
do_op_ensure_started_helper(Op, StartJQRetriesLeft) ->
    try
        Op()
    catch
        exit:{noproc, _} ->
            application:stop(jq),
            ok = application:ensure_started(jq),
            do_op_ensure_started_helper(Op, StartJQRetriesLeft - 1)
    end.

do_op_ensure_started(Op) ->
    do_op_ensure_started_helper(Op, 2).

start_recording(FileName) ->
    Op =
    fun() -> 
            Expect = [ok || _ <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
            Expect = [gen_server:call(port_server_by_id(Id),
                                      {start_recording,
                                       erlang:iolist_to_binary([FileName, "\0"])},
                                      infinity) ||
                      Id <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
            ok
    end,
    do_op_ensure_started(Op).


stop_recording() ->
    Op =
    fun() ->
            Expect = [ok || _ <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
            Expect = [gen_server:call(port_server_by_id(Id),
                                      stop_recording,
                                      infinity) ||
                      Id <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
            ok
    end,
    do_op_ensure_started(Op).

get_filter_program_lru_cache_max_size() ->
    Op =
    fun() ->
            gen_server:call(port_server(),
                            get_filter_program_lru_cache_max_size,
                            infinity)
    end,
    do_op_ensure_started(Op).

set_filter_program_lru_cache_max_size(NewSize)
  when is_integer(NewSize), NewSize >= 0, NewSize < 1073741824 ->
    Op =
    fun() ->
            gen_server:call(port_server(),
                            {set_filter_program_lru_cache_max_size, NewSize},
                            infinity)
    end,
    do_op_ensure_started(Op).

process_json(FilterProgram, JSONText) ->
    process_json(FilterProgram, JSONText, infinity).


process_json(FilterProgram, JSONText, TimeoutMs)
  when is_binary(FilterProgram), is_binary(JSONText) ->
    Op =
    fun() ->
            gen_server:call(port_server(),
                            {jq_process_json, FilterProgram, JSONText, TimeoutMs},
                            infinity)
    end,
    do_op_ensure_started(Op).

port_server_by_id(Id) ->
    persistent_term:get({?MODULE, Id}, jq_app_not_started).

port_server() ->
    port_server_by_id(erlang:phash2(self(),
                                    jq_port:nr_of_jq_port_servers())).

add_to_lookup_table(Id) ->
    persistent_term:put({?MODULE, Id}, self()).

remove_from_lookup_table(Id) ->
    persistent_term:erase({?MODULE, Id}).

init(Id) ->
    process_flag(trap_exit, true),
    Port = start_port_program(),
    State = #{
      port => Port,
      id => Id,
      processed_json_calls => 0,
      restart_period => application:get_env(jq, jq_port_restart_period, 1000000)
     },
    add_to_lookup_table(Id),
    {ok, State}.

port_program_path() ->
    PortProgramName = "erlang_jq_port",
    Path = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, PortProgramName]);
                _ ->
                    filename:join([priv, PortProgramName])
            end;
        Dir ->
            filename:join(Dir, PortProgramName)
    end,
    Path.

start_port_program() ->
    Port = erlang:open_port({spawn_executable, port_program_path()}, [{packet, 4}, binary]),
    true = is_port_alive(Port),
    Port.

receive_result_blobs(_, 0, SoFar) ->
    lists:reverse(SoFar);
receive_result_blobs(Port, Count, SoFar) ->
    case receive_msg_from_port(Port) of
        Data when is_binary(Data) ->
            receive_result_blobs(Port, Count - 1,
                                 [Data | SoFar])
    end.

state_port(State) ->
    maps:get(port, State).

state_id(State) ->
    maps:get(id, State).

state_restart_period(State) ->
    maps:get(restart_period, State).

state_processed_json_calls(State) ->
    maps:get(processed_json_calls, State).

is_port_alive(Port) ->
    PortAliveTimeout = application:get_env(jq, jq_port_is_alive_wait_timeout, 5 * 1000), %% 5 seconds
    send_msg_to_port(Port, <<"ping\0">>), 
    case receive_msg_from_port(Port, PortAliveTimeout) of
        <<"pong">> ->
            true;
        Other ->
            error({bad_ping_response, Other})
    end.

kill_port(Port) ->
    TimeToWaitForExitingConfirmation =
        application:get_env(jq, jq_port_ms_to_wait_for_exit_confirmation, 100),
    Port ! {self(), {command, <<"exit\0">>}},
    receive
        {Port, {data, <<"exiting">>}} -> 
            ok
    after TimeToWaitForExitingConfirmation ->
              PortInfoRes = erlang:port_info(Port, os_pid),
              case PortInfoRes of
                  {os_pid, OsPid} when is_integer(OsPid) ->
                      KillCmdFormat =
                      case os:type() of
                          {win32, _} ->
                              "taskkill /PID ~p /F";
                          {_, _ } -> 
                              %% We assume it is UNIX or some other OS
                              %% with an UNIX kill command.
                              "kill -9 ~p"
                      end,
                      %% This is theoretically unsafe as the process
                      %% might have died and the its PID got reused.
                      %% However, on mainstream systems it should not
                      %% be a problem as it is unlikely that the same
                      %% PID will get reused very quickly.
                      os:cmd(io_lib:format(KillCmdFormat, [OsPid]));
                  _ ->
                      %% Process should be closed already if we did
                      %% not get an integer
                      ok
              end,
              ok
    end,
    ok.

send_msg_to_port(Port, Msg) ->
    erlang:port_command(Port, Msg).

receive_msg_from_port(Port) ->
    receive_msg_from_port(Port, infinity).

receive_msg_from_port(Port, TimeoutMs) ->
    receive
        {Port, {data, Data}} ->
            Data;
        {'EXIT', Port, Reason} ->
            {error, port_exited_during_op, Port, Reason}
    after TimeoutMs ->
            {error, timeout_while_waiting_for_msg}
    end.

receive_ok_msg_from_port(Port, State) ->
    case receive_msg_from_port(Port) of
        <<"ok">> -> {reply, ok, State};
        SomethingElse -> {error, got_something_else_than_ok, SomethingElse}
    end.
 

misbehaving_port_program(State, ErrorClass, Reason) ->
    Ret =
        case Reason of
            {timeout_while_waiting_for_msg, TimeoutMs} ->
                ErrorMsgFormatTO = 
                    "jq program canceled as it took too long time to execute (timeout set to ~w ms)",
                ErrorMsgTO =
                    erlang:iolist_to_binary(io_lib:format(ErrorMsgFormatTO, [TimeoutMs])),
                logger:error(ErrorMsgTO),
                {error, {timeout, ErrorMsgTO}};
            _ ->
                ErrorMsgFormat = 
                    ("jq port program responded in unexpected way "
                     "(state = ~p error_class ~p reason ~p , restarting)"),
                ErrorMsg = erlang:iolist_to_binary(io_lib:format(ErrorMsgFormat,
                                                                 [State, ErrorClass, Reason])),
                logger:error(ErrorMsg),
                {error, {misbehaving_port_program, ErrorClass, Reason}}
        end,
    OldPort = state_port(State), 
    kill_port(OldPort),
    NewPort = start_port_program(),
    NewState = State#{port => NewPort},
    {reply, Ret, NewState}.

receive_process_json_result(Port, TimeoutMs) ->
    case receive_msg_from_port(Port, TimeoutMs) of
        <<"ok">> ->
            SizeStr = receive_msg_from_port(Port),
            NrOfItems = erlang:binary_to_integer(SizeStr),
            {ok, receive_result_blobs(Port, NrOfItems, [])};
        <<"error">> ->
            [ErrorType, ErrorMsg] = receive_result_blobs(Port, 2, []),
            {error, {erlang:binary_to_atom(ErrorType), ErrorMsg}};
        {error, timeout_while_waiting_for_msg} ->
            error({timeout_while_waiting_for_msg, TimeoutMs});
        Unexpected ->
            error({unexpected_response_from_port, Unexpected})
    end.

new_state_after_process_json(State) ->
    RestartPeriod = state_restart_period(State),
    NrOfCalls = state_processed_json_calls(State),
    case (NrOfCalls+1) rem RestartPeriod of
        0 -> 
            OldPort = state_port(State),
            kill_port(OldPort),
            NewPort = start_port_program(),
            State#{port => NewPort, processed_json_calls => NrOfCalls + 1};
        _ ->
            State#{processed_json_calls => NrOfCalls + 1}
    end.

handle_call({jq_process_json, FilterProgram, JSONText, TimeoutMs}, _From, State) ->
    Port = state_port(State),
    try
        send_msg_to_port(Port, <<"process_json\0">>), 
        send_msg_to_port(Port, FilterProgram), 
        send_msg_to_port(Port, JSONText), 
        {reply,
         receive_process_json_result(Port, TimeoutMs),
         new_state_after_process_json(State)}
    catch
        ErrorClass:Reason ->
            misbehaving_port_program(State, ErrorClass, Reason)
    end;
handle_call(stop, _From, State) ->
    kill_port(state_port(State)),
    {stop, normal, ok, State};
handle_call(get_filter_program_lru_cache_max_size, _From, State) ->
    Port = state_port(State),
    try
        send_msg_to_port(Port, <<"get_filter_program_lru_cache_max_size\0">>), 
        case receive_msg_from_port(Port) of
            CacheSizeStr ->
                {reply, erlang:binary_to_integer(CacheSizeStr), State}
        end
    catch
        ErrorClass:Reason ->
            misbehaving_port_program(State, ErrorClass, Reason)
    end;
handle_call({set_filter_program_lru_cache_max_size, NewSize}, _From, State) ->
    Port = state_port(State),
    try
        send_msg_to_port(Port, <<"set_filter_program_lru_cache_max_size\0">>),
        send_msg_to_port(Port, erlang:iolist_to_binary([erlang:integer_to_list(NewSize), "\0"])),
        receive_ok_msg_from_port(Port, State)
    catch
        ErrorClass:Reason ->
            misbehaving_port_program(State, ErrorClass, Reason)
    end;
handle_call({start_recording, FileName}, _From, State) ->
    Port = state_port(State),
    try
        send_msg_to_port(Port, <<"start_record_input\0">>),
        send_msg_to_port(Port, iolist_to_binary(FileName)),
        receive_ok_msg_from_port(Port, State)
    catch
        ErrorClass:Reason ->
            misbehaving_port_program(State, ErrorClass, Reason)
    end;
handle_call(stop_recording, _From, State) ->
    try
        Port = state_port(State),
        send_msg_to_port(Port, <<"stop_record_input\0">>),
        receive_ok_msg_from_port(Port, State)
    catch
        ErrorClass:Reason ->
            misbehaving_port_program(State, ErrorClass, Reason)
    end;
handle_call(UnknownMessage, From, State) ->
    logger:warning(io_lib:format("Unknown call message to jq port server: message = ~p from = ~p state = ~p",
                                 [UnknownMessage, From, State])),
    {noreply, State}.

handle_cast(UnknownMessage, State) ->
    logger:warning(io_lib:format("Unknown cast message sent to jq port server: message = ~p state = ~p",
                                 [UnknownMessage, State])),
    {noreply, State}.

terminate(_Reason, State) ->
    Port = state_port(State),
    kill_port(Port),
    Id = state_id(State),
    remove_from_lookup_table({?MODULE, Id}),
    ok.
    
handle_info({'EXIT', Port, Reason}, #{port := Port} = State) ->
    logger:error(io_lib:format("jq port program has died unexpectedly for reason ~p (state = ~p) \nTrying to restart...",
                               [Reason, State])),
    %% Let us try to start a new port
    NewPort = start_port_program(),
    {noreply, State#{port => NewPort}};
handle_info({'EXIT', Port, _Reason}, State) when is_port(Port) ->
    %% Flush message from old port 
    {noreply, State};
handle_info({OtherPort, {data, _Data}}, #{port := Port} = State)
  when is_port(OtherPort), OtherPort =/= Port ->
    %% Flush message from old port 
    {noreply, State};
handle_info(UnknownMessage, State) ->
    logger:info(io_lib:format("Unhandled message sent to jq port server: message ~p state: ~p",
                              [UnknownMessage, State])),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    OldPort = state_port(State),
    kill_port(OldPort),
    NewPort = start_port_program(),
    NewState = State#{port => NewPort},
    {ok, NewState}.

nr_of_jq_port_servers() ->
    persistent_term:get(jq_nr_of_jq_port_servers, erlang:system_info(schedulers)).

set_nr_of_jq_port_servers(NrOfJqPortServers)
  when is_integer(NrOfJqPortServers), NrOfJqPortServers > 0, NrOfJqPortServers =< 8192 ->
    case lists:any(fun({jq,_,_}) -> true; (_) -> false end, application:which_applications()) of
        true -> logger:warning("Changing the number of jq port servers while the jq application is running may not be safe");
        false -> ok
    end,
    persistent_term:put(jq_nr_of_jq_port_servers, NrOfJqPortServers).
