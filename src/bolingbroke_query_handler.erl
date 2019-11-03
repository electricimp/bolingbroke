-module(bolingbroke_query_handler).
-export([init/2]).

-define(DEFAULT_INTERVAL_MS, 10000).

init(Req, _Opts) ->
    Qs = cowboy_req:qs(Req),
    Patterns = [binary_to_list(P) || P <- binary:split(Qs, <<"&">>, [global])],
    JSON = bolingbroke_query:create_json(Patterns),

    Headers = #{<<"content-type">> => <<"application/json">>},
    Req2 = cowboy_req:reply(200, Headers, JSON, Req),
    {ok, Req2, no_state}.
