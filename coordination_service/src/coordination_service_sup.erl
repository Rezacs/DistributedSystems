%%%-------------------------------------------------------------------
%% @doc coordination_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(coordination_service_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %% Create the ETS table for server registry
    ets:new(server_registry, [named_table, public, set, {keypos, 1}]),
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

