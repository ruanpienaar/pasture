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
    {ok,Master} = application:get_env(pasture, mnesia_master),
    mnesia_start(Master),
    %% Restart the connection if it fails.
    Ref =
        ibrowse:send_req(
            "http://stream.meetup.com/2/rsvps",[],get,[],
            [ {stream_chunk_size,1024 * 2},
              {stream_to,pasture_meetup}
            ], infinity),
    ok = gen_server:call(pasture_meetup,Ref),
    %%{ok,_RanchListenerPid} = pasture_web:start(),
    ok.

mnesia_start(Master) ->
    case Master =:= node() of
        true ->
            Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
            io:format("ALive nodes() : ~p\n",[nodes()]),
            AliveNodes = test_nodes(Nodes,Nodes),
            io:format("Trying to install schema on ~p\n",[Nodes]),
            stopped = mnesia:stop(),
            ok = mnesia:set_master_nodes([Master]),
            case mnesia:create_schema(lists:reverse(Nodes)) of
                ok ->
                    [ rpc:call(N, mnesia, start, []) || N <- Nodes ];
                {error,{_Node,{already_exists,_Node}}} ->
                    [ rpc:call(N, mnesia, start, []) || N <- Nodes ];
                {error,{RemoteNode,{_,RemoteNode,nodedown}}} ->
                    throw(toys_out_the_cot)
            end,
            try
                Info = mnesia:table_info(pasture_meetup,attributes)
            catch
                throw:_ ->
                    throw(explode);
                C:E ->
                    {atomic,ok} =
                        mnesia:create_table(
                                pasture_meetup,
                                [{type,set},
                                 {disc_only_copies,Nodes},
                                 {attributes,record_info(fields, pasture_meetup)}
                               ])
            end,
            ?INFO("Waiting for tables...\n"),
            ok = mnesia:wait_for_tables([pasture_meetup],infinity);
        false ->
            ?INFO("Not master, just booting...\n")
    end.

test_nodes(Nodes,[]) ->
    Nodes;
test_nodes(Nodes,[H|T]) ->
    case net_adm:ping(H) of
        pong ->
            test_nodes(Nodes,T);
        pang ->
            timer:sleep(500),
            %% io:format("Waiting for nodes to become alive...",[]),
            test_nodes(Nodes,Nodes)
    end.

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
    io:format("Starting ... ~p ... \n\n",[App]),
    case application:start(App) of
        ok ->
            start_deps(T);
        {error,{not_started,DepApp}} ->
            io:format("Dependancy ... ~p ... needed \n\n",[DepApp]),
            start_deps([DepApp|[App|T]])
    end.
