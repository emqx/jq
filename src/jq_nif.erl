
-module(jq_nif).

-export([
           process_json/2
         , process_json/3
         , set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0
         , timeout_resource/1]
       ).

-on_load(init/0).

-define(APPNAME, jq).
-define(VERSION, 1).
-define(LIBNAME, "jq_nif").

process_json(_, _) ->
    not_loaded(?LINE).

create_jq_resource() ->
    not_loaded(?LINE).

cancel_jq_resource(_JQResource) ->
    not_loaded(?LINE).

process_json_with_jq_resource(_FilterProgram, _JSONText, _JQResource) ->
    not_loaded(?LINE).


timeout_resource(JQResource) ->
    case cancel_jq_resource(JQResource) of
        ok -> ok;
        retry ->
            % Attempt to give dirty schedulers some time before continuing
            timer:sleep(1),
            timeout_resource(JQResource)
    end.

process_json(FilterProgram, JSONText, TimeoutMs) ->
    JQResource  = create_jq_resource(),
    {ok, TRef} = timer:apply_after(TimeoutMs, jq_nif, timeout_resource, [JQResource]),
    Res = process_json_with_jq_resource(FilterProgram, JSONText, JQResource),
    timer:cancel(TRef),
    case Res of
        {error, {timeout, _}} ->
            ErrorMsgFormatTO = 
                "jq program canceled as it took too long time to execute (timeout set to ~w ms)",
            ErrorMsgTO =
                erlang:iolist_to_binary(io_lib:format(ErrorMsgFormatTO, [TimeoutMs])),
            logger:error(ErrorMsgTO),
            {error, {timeout, ErrorMsgTO}};
        Term -> Term
    end.

set_filter_program_lru_cache_max_size(_) ->
    not_loaded(?LINE).

get_filter_program_lru_cache_max_size() ->
    not_loaded(?LINE).

init() ->
    LibNameAndVersion = ?LIBNAME ++ erlang:integer_to_list(?VERSION),
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, LibNameAndVersion]);
                _ ->
                    filename:join([priv, LibNameAndVersion])
            end;
        Dir ->
            filename:join(Dir, LibNameAndVersion)
    end,
    %% Load the jq application here since it needs to be loaded so
    %% we can read its properties
    application:load(jq),
    CacheMaxSize =
        application:get_env(jq, jq_filter_program_lru_cache_max_size, 500),
    JQNifConfig =
        #{filter_program_lru_cache_max_size => CacheMaxSize,
          version => ?VERSION},
    erlang:load_nif(SoName, JQNifConfig).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

