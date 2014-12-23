-module(pasture_app).

-behaviour(application).

-export([start/1]).
-export([start/2, stop/1]).

-include("../include/pasture.hrl").

start(_ArgsList) ->
    ?INFO("Db init...\n"),
    ok = pasture_db:init(),
    ?INFO("start deps...\n"),
    ok = start_deps([pasture, {reloader,start}]),
    %%{ok,_RanchListenerPid} = pasture_web:start(),
    ok.

start(_StartType, _StartArgs) ->
    case pasture_sup:start_link() of
        {ok,SupPid} ->
            {ok,SupPid};
        E ->
            E
    end.

stop(_State) ->
    ok.

start_deps([]) ->
    ok;
start_deps([{M,F}|T]) ->
    try M:F() catch _:_ -> ok end,
    start_deps(T);
start_deps([{M,F,A}|T]) ->
    try M:F(A) catch _:_ -> ok end,
    start_deps(T);
start_deps([App|T]) ->
    lager:info("Starting ... ~p ... \n\n",[App]),
    case application:start(App) of
        ok ->
            start_deps(T);
        {error,{not_started,DepApp}} ->
            lager:info("Dependancy ... ~p ... needed \n\n",[DepApp]),
            start_deps([DepApp|[App|T]])
    end.