-module(jq).

-export([  parse/2
         , set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0]).
-on_load(init/0).

-define(APPNAME, jq).
-define(LIBNAME, jq).

parse(_, _) ->
    not_loaded(?LINE).

set_filter_program_lru_cache_max_size(_) ->
    not_loaded(?LINE).

get_filter_program_lru_cache_max_size() ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    %% Start the jq application here since it needs to be started so
    %% we can read its properties
    application:load(jq),
    CacheMaxSize =
        application:get_env(jq, jq_filter_program_lru_cache_max_size, 500),
    JQNifConfig =
        #{filter_program_lru_cache_max_size => CacheMaxSize},
    erlang:load_nif(SoName, JQNifConfig).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

