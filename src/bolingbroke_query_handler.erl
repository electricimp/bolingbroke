-module(bolingbroke_query_handler).
-export([init/3, handle/2, terminate/3]).

-define(DEFAULT_INTERVAL_MS, 10000).

init(_Type, Req, _Opts) ->
    State = undefined,
    {ok, Req, State}.

handle(Req, State) ->
    {Qs, Req2} = cowboy_req:qs(Req),
    Patterns = [binary_to_list(P) || P <- binary:split(Qs, <<"&">>, [global])],
    JSON = bolingbroke_query:create_json(Patterns),

    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, Req3} = cowboy_req:reply(200, Headers, JSON, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
