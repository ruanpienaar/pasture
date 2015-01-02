-module(pasture_db).

-export([init/0,
         json_to_recs/1
         ]).

-include("../include/pasture.hrl").

init() ->
    Master = application:get_env(pasture, mnesia_master, node()),
    MnesiaTbls=[pasture_event,pasture_group,pasture_venue,pasture_member],
    ok = init_master(MnesiaTbls,Master).

init_master(MnesiaTbls,MasterNode) when MasterNode == node() ->
    Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
    ok = wait_for_cluster(Nodes),
    ?INFO("Stopping mnesia on ~p...\n",[Nodes]),
    [ rpc:call(N, mnesia, stop, []) || N <- Nodes ],
    ?INFO("Trying to create schema...\n"),
    case mnesia:create_schema(Nodes) of
        ok ->
            ?INFO("Schema Created...\n"),
            [ begin
                rpc:call(N, mnesia, start, []) %,
                %% rpc:call(N, mnesia, wait, []),
            end || N <- Nodes ],
            % pasture_ids:create_table(Nodes),
            [ begin
                Tbl:create_table(Nodes)
                % ,
                % pasture_ids:create_id(Tbl)
              end || Tbl <- MnesiaTbls ],
            ok = mnesia:wait_for_tables(MnesiaTbls,infinity),
            ok = mnesia:set_master_nodes([MasterNode]),
            % [ rpc:call(N, pasture_app, start_deps, [[pasture]])
            %     || N <- Nodes ],
            ok;
        {error,{_Node,{already_exists,_Node}}} ->
            ?INFO("Schema Already Created...\n"),
            [ rpc:call(N, mnesia, start, []) || N <- Nodes ],
            [ rpc:call(N, mnesia, wait_for_tables, [MnesiaTbls,infinity])
                || N <- Nodes ],
            ok = mnesia:wait_for_tables(MnesiaTbls,infinity),
            % [ rpc:call(N, pasture_app, start_deps, [[pasture]])
            %     || N <- Nodes ],
            ok;
        {error,{RemoteNode,{_,RemoteNode,nodedown}}} ->
            ?INFO("Schema issue {error,{~p,{_,~p,nodedown}}}...\n",
                [RemoteNode]),
            erlang:exit(mnesia_nodes_unavailable)
    end;
init_master(_MnesiaTbls,_MasterNode) ->
    ok.

wait_for_cluster(Nodes) ->
    checK_node_status(false,Nodes).

checK_node_status(true,_) ->
    ?INFO("All nodes ready...\n\n"),
    ok;
checK_node_status(false,Nodes) ->
    ?INFO("checK_node_status ~p...\n\n",[Nodes]),
    timer:sleep(100),

    F = fun(Node) ->
        net_adm:ping(Node),
        case rpc:call(Node,application,get_application,[mnesia]) of
                {badrpc,nodedown} ->
                    false;
                undefined ->
                    false;
                {ok,mnesia} ->
                    true
            end
    end,
    checK_node_status(lists:all(F, Nodes),Nodes).

json_to_recs([]) ->
    ok;
json_to_recs([{<<"event">>,Objs}|T]) ->
    ok = pasture_event:new(Objs),
    %% ok = pasture_db_batch:add({pasture_event,Objs}),
    json_to_recs(T);
json_to_recs([{<<"group">>,Objs}|T]) ->
    ok = pasture_group:new(Objs),
    %% ok = pasture_db_batch:add({pasture_group,Objs}),
    json_to_recs(T);
json_to_recs([{<<"member">>,Objs}|T]) ->
    ok = pasture_member:new(Objs),
    %% ok = pasture_db_batch:add({pasture_member,Objs}),
    json_to_recs(T);
json_to_recs([{<<"venue">>,Objs}|T]) ->
    ok = pasture_venue:new(Objs),
    %% ok = pasture_db_batch:add({pasture_venue,Objs}),
    json_to_recs(T);
json_to_recs([_H|T]) ->
    json_to_recs(T).
