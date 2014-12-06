-module(pasture_app).

-behaviour(application).

%% Dev
-export([start/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% Dev
start(ArgsList) ->
    ok = start_deps([pasture, {reloader,start}]),
    %% Not needed at the mo
    %%{ok,_RanchListenerPid} = pasture_web:start(),
    start(dev, ArgsList).

start(_StartType, _StartArgs) ->
    case pasture_sup:start_link() of
        {ok,SupPid} ->
            {ok,SupPid};
        _ ->
            oh_no
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
    io:format("Starting ... ~p ... \n\n",[App]),
    case application:start(App) of
        ok ->
            start_deps(T);
        {error,{not_started,DepApp}} ->
            io:format("Dependancy ... ~p ... needed \n\n",[DepApp]),
            start_deps([DepApp|[App|T]])
    end.
