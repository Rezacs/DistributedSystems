-module(coordination_handler).
-behaviour(cowboy_handler).
-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    case {Path, Method} of
        {<<"/register">>, _} ->
            handle_register(Method, Req0, State);
        {<<"/deregister">>, <<"POST">>} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case catch jsx:decode(Body, [return_maps]) of
                {'EXIT', _} ->
                    reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State);
                Map when is_map(Map) ->
                    ServerId = maps:get(<<"server_id">>, Map, undefined),
                    case ServerId of
                        undefined ->
                            reply_json(400, <<"{\"error\": \"Missing server_id\"}">>, Req1, State);
                        _ ->
                            ets:delete(server_registry, ServerId),
                            reply_json(200, <<"{\"status\": \"deregistered\"}">>, Req1, State)
                    end;
                _ ->
                    reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State)
            end;
        _ ->
            reply_json(405, <<"{\"error\": \"Method Not Allowed or Unknown Path\"}">>, Req0, State)
    end.

handle_register(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State);
        Map when is_map(Map) ->
            ServerId = maps:get(<<"server_id">>, Map, undefined),
            Host = maps:get(<<"host">>, Map, undefined),
            Port = maps:get(<<"port">>, Map, undefined),
            case {ServerId, Host, Port} of
                {undefined, _, _} ->
                    reply_json(400, <<"{\"error\": \"Missing server_id\"}">>, Req1, State);
                {_, undefined, _} ->
                    reply_json(400, <<"{\"error\": \"Missing host\"}">>, Req1, State);
                {_, _, undefined} ->
                    reply_json(400, <<"{\"error\": \"Missing port\"}">>, Req1, State);
                _ ->
                    ets:insert(server_registry, {ServerId, Host, Port}),
                    reply_json(200, <<"{\"status\": \"registered\"}">>, Req1, State)
            end;
        _ ->
            reply_json(400, <<"{\"error\": \"Invalid JSON\"}">>, Req1, State)
    end;

handle_register(<<"GET">>, Req0, State) ->
    Servers = ets:tab2list(server_registry),
    JsonList = [ #{<<"server_id">> => Id, <<"host">> => Host, <<"port">> => Port} || {Id, Host, Port} <- Servers ],
    Body = jsx:encode(JsonList),
    reply_json(200, Body, Req0, State);

handle_register(_, Req0, State) ->
    reply_json(405, <<"{\"error\": \"Method Not Allowed\"}">>, Req0, State).

reply_json(Code, Body, Req, State) ->
    {ok, Req2} = cowboy_req:reply(
        Code,
        #{<<"content-type">> => <<"application/json">>},
        Body,
        Req
    ),
    {ok, Req2, State}.

