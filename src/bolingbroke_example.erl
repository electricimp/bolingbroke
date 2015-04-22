-module(bolingbroke_example).
-export([start_link/0]).
-export([init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    folsom_metrics:new_gauge({bolingbroke, example, gauge}),
    folsom_metrics:new_counter({bolingbroke, example, counter}),
    folsom_metrics:new_histogram({bolingbroke, example, histogram}),
    State = 50,
    loop(State).

loop(State) ->
    Value = min(100, max(0, State + random:uniform(20) - 10)),
    folsom_metrics:notify({bolingbroke, example, gauge}, Value),

    case random:uniform() > 0.2 of
        true ->
            folsom_metrics:notify({bolingbroke, example, counter}, {inc, 1});
        _ ->
            folsom_metrics:notify({bolingbroke, example, counter}, {dec, 1})
    end,

    folsom_metrics:notify({bolingbroke, example, histogram}, random:uniform(100)),

    timer:sleep(1000),
    State2 = Value,
    loop(State2).
