-module(coordination_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% ✅ Create ETS table ONCE at application startup
    ets:new(game_sessions, [named_table, public, set, {keypos, 1}]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/register", coordination_handler, []},
            {"/deregister", coordination_handler, []},
            {"/servers", coordination_handler, []},
            {"/savegame", coordination_handler, []},
            {"/getgame/[...]", coordination_handler, []},
            {"/heartbeat", coordination_handler, []}    %% <--- ADD THIS LINE
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    coordination_service_sup:start_link().

stop(_State) ->
    ok.

