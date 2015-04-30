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
    JSON = create_json(Patterns),
    Chunk = ["data: ", JSON, "\r\n",
             "\r\n"],
    ok = cowboy_req:chunk(Chunk, Req),

    IntervalMs = application:get_env(bolingbroke, update_interval_ms, ?DEFAULT_INTERVAL_MS),
    erlang:send_after(IntervalMs, self(), update),

    {loop, Req2, State}.

create_json(undefined) ->
    Time = unix_time_ms(os:timestamp()),
    io_lib:format(
      "{\"t\":~w,\"m\":[]}",
      [Time]);

create_json(Patterns) ->
    Time = unix_time_ms(os:timestamp()),

    Pred = fun({Name, _Value}) ->
                   lists:any(
                     fun(Pattern) ->
                             string:str(Name, Pattern) =/= 0
                     end, Patterns)
           end,
    Metrics = lists:filter(Pred, get_stats()),
    MetricsJ = lists:map(
                 fun({Name, Value}) ->
                         lists:flatten(
                           io_lib:format("{\"n\":\"~s\",\"v\":~w}",
                                         [stringify(Name), Value]))
                 end, Metrics),

    io_lib:format(
      "{\"node\":\"~s\", \"t\":~w,\"m\":[~s]}",
      [node(), Time, string:join(MetricsJ, ",")]).

get_stats() ->
    Memory = expand0(folsom_vm_metrics:get_memory(), [memory, vm]),
    Stats = expand0(folsom_vm_metrics:get_statistics(), [vm]),
    Metrics = folsom_metrics:get_metrics_info(),
    Memory ++ Stats ++ lists:flatmap(fun expand_metric/1, Metrics).


%% @doc Returns `[]' for unknown (skipped) metricts.
-spec expand_metric({Name, Opts}) -> [Metric] when
      Metric :: {K::string(), V::string()},
      Name :: term(),
      Opts :: [proplists:property()].
expand_metric({Name, Opts}) ->
    case proplists:get_value(type, Opts) of
        undefined -> [];
        histogram ->
            Stats = folsom_metrics:get_histogram_statistics(Name),
            M = proplists:delete(histogram, Stats),
            expand0(M, [Name]);
        Type ->
            case lists:member(Type,
                              [counter, gauge, meter, spiral, meter_reader, duration]) of
                true ->
                    M = folsom_metrics:get_metric_value(Name),
                    expand0(M, [Name]);
                false -> []
            end
    end;
expand_metric(_) ->
    [].

expand0(M, NamePrefix) -> lists:flatten(expand(M, NamePrefix)).

expand({K, X}, NamePrefix) ->
    expand(X, [K | NamePrefix]);
expand([_|_] = Xs, NamePrefix) ->
    [expand(X, NamePrefix) || X <- Xs];
expand(X, NamePrefix) ->
    K = string:join(lists:map(fun stringify/1, lists:reverse(NamePrefix)), "."),
    [{K, X}].

stringify(X) when is_list(X) -> X;
stringify(X) when is_atom(X) -> atom_to_list(X);
stringify(X) when is_integer(X) -> integer_to_list(X);
stringify(X) when is_float(X) -> float_to_list(X);
stringify(X) when is_binary(X) -> binary_to_list(X);
stringify(X) when is_tuple(X) ->
    string:join([stringify(A) || A <- tuple_to_list(X)], ".").

unix_time_ms({Me, Se, Mi}) ->
    (Me * 1000 * 1000 * 1000) + (Se * 1000) + trunc(Mi / 1000).

terminate(_Reason, _Req, _State) ->
    ok.
