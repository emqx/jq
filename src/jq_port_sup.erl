
%%%-------------------------------------------------------------------
%% @doc Supervisor for jq port servers
%% @end
%%%-------------------------------------------------------------------

-module(jq_port_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1, %% Allowed restarts within period 
                 period => 20}, %% Seconds
    ChildSpecs = [child_spec(Id) || Id <- lists:seq(0, jq_port:nr_of_jq_port_servers() - 1)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

child_spec(Id) ->
    #{id => Id,
      start => {jq_port, start_link, [Id]},
      restart => transient,
      shutdown => 5000,
      modules => [jq_port] 
     }.

