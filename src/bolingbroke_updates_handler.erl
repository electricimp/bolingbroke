-module(bolingbroke_updates_handler).
-export([init/2, info/3, terminate/3]).

-define(DEFAULT_INTERVAL_MS, 10000).

init(Req0, _Opts) ->
    Headers = #{<<"content-type">> => <<"text/event-stream">>},
    Req = cowboy_req:stream_reply(200, Headers, Req0),

    self() ! update,

    State = undefined,
    {cowboy_loop, Req, State}.

info(update, Req, State) ->
    Qs = cowboy_req:qs(Req),
    Patterns = [binary_to_list(P) || P <- binary:split(Qs, <<"&">>, [global])],
    JSON = bolingbroke_query:create_json(Patterns),
    ok = cowboy_req:stream_events(#{ id => next_id(), data => JSON }, nofin, Req),

    IntervalMs = application:get_env(bolingbroke, update_interval_ms, ?DEFAULT_INTERVAL_MS),
    erlang:send_after(IntervalMs, self(), update),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

next_id() ->
    integer_to_list(erlang:unique_integer([positive, monotonic]), 16).
