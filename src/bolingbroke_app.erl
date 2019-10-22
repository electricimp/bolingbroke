-module(bolingbroke_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = application:get_env(bolingbroke, port, 18360),
    Routes = [
              {'_', [
                     {"/", cowboy_static, {priv_file, bolingbroke, "static/index.html"}},
                     {"/updates", bolingbroke_updates_handler, []},
                     {"/query", bolingbroke_query_handler, []},

                     {"/js/[...]", cowboy_static, {priv_dir, bolingbroke, "js"}},
                     {"/css/[...]", cowboy_static, {priv_dir, bolingbroke, "css"}}
                    ]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(bolingbroke_http,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    bolingbroke_sup:start_link().

stop(_State) ->
    ok.
