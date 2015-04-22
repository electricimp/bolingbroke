-module(bolingbroke).
-export([start/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @doc This is the dev entry point, so it seems like a good place to start some fake metrics.
start() ->
    {ok, _} = application:ensure_all_started(bolingbroke),
    {ok, _} = supervisor:start_child(bolingbroke_sup,
                                     {foo, {bolingbroke_example, start_link, []},
                                      permanent, 5000, worker, [bolingbroke_example]}),
    ok.
