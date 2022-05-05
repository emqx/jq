
-module(jq_nif).

-export([
           process_json/2
         , set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0]
       ).

-on_load(init/0).

-define(APPNAME, jq).
-define(VERSION, 1).
-define(LIBNAME, "jq_nif").

process_json(_, _) ->
    not_loaded(?LINE).

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

