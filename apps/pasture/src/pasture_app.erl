-module(pasture_app).

-behaviour(application).

-export([start/1, start/2,
         stop/1,
         start_deps/1
        ]).

-include("../include/pasture.hrl").

start(_ArgsList) ->
    mnesia:set_debug_level(verbose),
    ok = start_deps([lager, ssl,
        cowboy, oauth, ibrowse, mnesia]),
    ?INFO("Db init...\n"),
    %% TODO: temp fix, should use rebar
    application:load(pasture),
    ok = pasture_db:init(),
    ok = start_deps([pasture]),
    {ok,_RanchListenerPid} = pasture_web:start(),
    ok.

start(_StartType, _StartArgs) ->
    case pasture_sup:start_link() of
        {ok,SupPid} ->
            %% TODO: mmm... find a beter way of doing this ......
            {ok,_} = pasture_db_sup:start_link(),
            {ok,C} = application:get_env(pasture, meetup_chunk_count),
             pasture_sup:start_children(C),
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
            start_deps([DepApp|[App|T]]);
        {error,{already_started,App}} ->
            start_deps(T)
    end.