-module(pasture_app).

-behaviour(application).

%% Dev
-export([start/1]).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/pasture.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% Dev
start(ArgsList) ->
    ok = start_deps([pasture, {reloader,start}]),

    Nodes = application:get_env(pasture, mnesia_nodes, [node()]),

    stopped = mnesia:stop(),
    mnesia:create_schema([Nodes]),
    ok = mnesia:start(),
    mnesia:create_table(
        pasture_meetup,
        [{type,set},
         {disc_only_copies,[Nodes]},
         {attributes,record_info(fields, pasture_meetup)}
       ]),

    %% Not needed at the mo
    %%{ok,_RanchListenerPid} = pasture_web:start(),
    start(dev, ArgsList).

start(_StartType, _StartArgs) ->
    case pasture_sup:start_link() of
        {ok,SupPid} ->
            Ref =
                ibrowse:send_req(
                    "http://stream.meetup.com/2/rsvps",[],get,[],
                    [
                      {preserve_chunked_encoding,true},
                      {stream_chunk_size,1024 * 24},
                      %% {save_response_to_file,"response.txt"},
                      {stream_to,pasture_meetup}
                    ], infinity),
            ok = gen_server:call(pasture_meetup,Ref),

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
    io:format("Starting ... ~p ... \n\n",[App]),
    case application:start(App) of
        ok ->
            start_deps(T);
        {error,{not_started,DepApp}} ->
            io:format("Dependancy ... ~p ... needed \n\n",[DepApp]),
            start_deps([DepApp|[App|T]])
    end.
