-module(coordination_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    case {Path, Method} of
        {<<"/register">>, <<"POST">>} ->
            handle_register(Req0, State);
        {<<"/heartbeat">>, <<"POST">>} ->
            handle_heartbeat(Req0, State);
        {<<"/servers">>, <<"GET">>} ->
            handle_servers(Req0, State);
        {<<"/deregister">>, <<"POST">>} ->
            handle_deregister(Req0, State);
        {<<"/savegame">>, <<"POST">>} ->
            handle_savegame(Req0, State);
        {<<"/getgame">>, <<"GET">>} ->
            handle_getgame(Req0, State);
        _ ->
            reply_json(405, <<"{\"error\": \"Method Not Allowed or Unknown Path\"}">>, Req0, State)
    end.

handle_register(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State);
        Map when is_map(Map) ->
            ServerId = maps:get(<<"server_id">>, Map, undefined),
            Host = maps:get(<<"host">>, Map, undefined),
            Port = maps:get(<<"port">>, Map, undefined),
            %% Debug print:
            io:format("Received register for ~p~n", [ServerId]),
            Now = erlang:system_time(second),
            case {ServerId, Host, Port} of
                {undefined, _, _} -> reply_json(400, <<"{\"error\": \"Missing server_id\"}">>, Req1, State);
                {_, undefined, _} -> reply_json(400, <<"{\"error\": \"Missing host\"}">>, Req1, State);
                {_, _, undefined} -> reply_json(400, <<"{\"error\": \"Missing port\"}">>, Req1, State);
                _ ->
                    ets:insert(server_registry, {ServerId, Host, Port, Now}),
                    reply_json(200, <<"{\"status\": \"registered\"}">>, Req1, State)
            end;
        _ -> reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State)
    end.

handle_heartbeat(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State);
        Map when is_map(Map) ->
            ServerId = maps:get(<<"server_id">>, Map, undefined),
            %% Debug print:
            io:format("Received heartbeat for ~p~n", [ServerId]),
            Now = erlang:system_time(second),
            case ets:lookup(server_registry, ServerId) of
                [{ServerId, Host, Port, _OldLastSeen}] ->
                    ets:insert(server_registry, {ServerId, Host, Port, Now}),
                    reply_json(200, <<"{\"status\": \"heartbeat ok\"}">>, Req1, State);
                [] ->
                    reply_json(404, <<"{\"error\": \"Server not registered\"}">>, Req1, State)
            end;
        _ -> reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State)
    end.


handle_servers(Req, State) ->
    Now = erlang:system_time(second),
    Timeout = 10, %% seconds without heartbeat before considering a server dead
    Servers0 = ets:tab2list(server_registry),
    %% Filter out servers whose last_seen is too old
    Servers = [S || S = {_, _, _, LastSeen} <- Servers0, Now - LastSeen =< Timeout],
    %% Optionally, remove dead ones from ETS:
    [ets:delete(server_registry, Id) || {Id, _, _, LastSeen} <- Servers0, Now - LastSeen > Timeout],
    JsonList = [ #{<<"server_id">> => Id, <<"host">> => Host, <<"port">> => Port, <<"last_seen">> => LastSeen}
                   || {Id, Host, Port, LastSeen} <- Servers ],
    Body = jsx:encode(JsonList),
    reply_json(200, Body, Req, State).

handle_deregister(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State);
        Map when is_map(Map) ->
            ServerId = maps:get(<<"server_id">>, Map, undefined),
            case ServerId of
                undefined -> reply_json(400, <<"{\"error\": \"Missing server_id\"}">>, Req1, State);
                _ -> ets:delete(server_registry, ServerId),
                     reply_json(200, <<"{\"status\": \"deregistered\"}">>, Req1, State)
            end;
        _ -> reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State)
    end.

%% Save game state
handle_savegame(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} -> reply_json(400, <<"{\"error\":\"Invalid JSON\"}">>, Req1, State);
        Map when is_map(Map) ->
            GameId = maps:get(<<"game_id">>, Map, undefined),
            case GameId of
                undefined -> reply_json(400, <<"{\"error\":\"Missing game_id\"}">>, Req1, State);
                _ -> ets:insert(game_sessions, {GameId, Map}),
                     reply_json(200, <<"{\"status\": \"saved\"}">>, Req1, State)
            end;
        _ -> reply_json(400, <<"{\"error\":\"Invalid JSON\"}">>, Req1, State)
    end.

%% Get game state
handle_getgame(Req, State) ->
    {GameId, Req1} = cowboy_req:binding(game_id, Req),
    case ets:lookup(game_sessions, GameId) of
        [{_, Game}] -> reply_json(200, jsx:encode(Game), Req1, State);
        [] -> reply_json(404, <<"{\"error\":\"Not found\"}">>, Req1, State)
    end.


reply_json(Code, Body, Req, State) ->
        Req2 = cowboy_req:reply(
        Code,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req
    ),
    {ok, Req2, State}.
