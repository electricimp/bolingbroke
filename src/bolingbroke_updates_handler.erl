-module(bolingbroke_updates_handler).
-export([init/3, info/3, terminate/3]).

-define(DEFAULT_INTERVAL_MS, 10000).

init(_Type, Req, _Opts) ->
    Headers = [{<<"content-type">>, <<"text/event-stream">>}],
    {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),

    self() ! update,

    State = undefined,
    {loop, Req2, State}.

info(update, Req, State) ->
    {Qs, Req2} = cowboy_req:qs(Req),
    Patterns = [binary_to_list(P) || P <- binary:split(Qs, <<"&">>, [global])],
    JSON = bolingbroke_query:create_json(Patterns),
    Chunk = ["data: ", JSON, "\r\n",
             "\r\n"],
    ok = cowboy_req:chunk(Chunk, Req),

    IntervalMs = application:get_env(bolingbroke, update_interval_ms, ?DEFAULT_INTERVAL_MS),
    erlang:send_after(IntervalMs, self(), update),

    {loop, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
