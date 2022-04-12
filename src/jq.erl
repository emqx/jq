
%%%-------------------------------------------------------------------
%% @doc This module provides the public API for the Erlang JSON library
%% @end
%%%-------------------------------------------------------------------


-module(jq).

-export([
           process_json/2
         , set_filter_program_lru_cache_max_size/1
         , get_filter_program_lru_cache_max_size/0
         , implementation_module/0
         , set_implementation_module/1
        ]).
-on_load(init/0).


%% @doc function that uses jq (https://stedolan.github.io/jq) to transform JSON data.
%%
%% @param FilterProgram jq filter program that will be used transform JSONText.
%% See the jq documentation (https://stedolan.github.io/jq/manual/v1.6) for how
%% to write valid filter programs.
%%
%% @param JSONText Binary or iodata containing a valid JSON object
%%
%% @returns ok tuple with list of binaries with resulting JSON objects or error
%% tuple in case of an error


-spec process_json(FilterProgram, JSONText) -> Result when
    FilterProgram :: iodata(),
    JSONText :: iodata(),
    Result :: {ok, [binary()]} | {error, Reason},
    Reason :: term().

process_json(FilterProgram, JSONText)
  when is_binary(FilterProgram), is_binary(JSONText) ->
    Mod = implementation_module(),
    Mod:process_json(FilterProgram, JSONText);
process_json(FilterProgram, JSONText) ->
    process_json(erlang:iolist_to_binary(FilterProgram),
                 erlang:iolist_to_binary(JSONText)). 

%% @doc Get the implementation module that is currently used.

-spec implementation_module() -> 'jq_port' | 'jq_nif'.

implementation_module() ->
    persistent_term:get(jq_implementation_module).

%% @doc Set the implementation module that shall be used.

-spec set_implementation_module('jq_port' | 'jq_nif') -> 'ok'.

set_implementation_module(Module) when Module =:= 'jq_port'; Module =:= 'jq_nif' ->
    persistent_term:put(jq_implementation_module, Module),
    ok.

%% @doc Get the max size of the LRU cache for filter programs

-spec get_filter_program_lru_cache_max_size() -> non_neg_integer().

get_filter_program_lru_cache_max_size() ->
    Mod = implementation_module(),
    Mod:get_filter_program_lru_cache_max_size().

%% @doc Set the max size of the LRU cache for filter programs

-spec set_filter_program_lru_cache_max_size(0..1073741824) -> ok.

set_filter_program_lru_cache_max_size(NewSize)
  when is_integer(NewSize), NewSize >= 0, NewSize < 1073741824 ->
    Mod = implementation_module(),
    Mod:set_filter_program_lru_cache_max_size(NewSize).

init() ->
    %% Load the jq application here since it needs to be loaded so
    %% we can read its properties
    application:load(jq),
    JQImplementation =
        application:get_env(jq, jq_implementation_module, jq_port),
    set_implementation_module(JQImplementation).
