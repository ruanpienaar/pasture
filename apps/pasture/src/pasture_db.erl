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
    F = fun(Node) -> net_kernel:connect(Node) end,
    ok = wait_for_cluster(Nodes,F),
    ?INFO("Checking cluster schema...\n"),
    %% TODO: use
    %% mnesia:table_info(schema,disc_copies).
    %% To check that
    F2 = fun(Node,{SNodes,NoSNodes}) ->
        %% TODO: check both...
        %% case rpc:call(Node,mnesia,table_info,[schema, all]) of
        case rpc:call(Node,mnesia,table_info,[schema, disc_copies]) of

            {aborted,{no_exists,schema,disc_copies}} ->
                %% If deleted on a node....
                {SNodes,[Node|NoSNodes]};
            {badrpc,{'EXIT',{aborted,{no_exists,schema,all}}}} ->
                %% Case nobody has schema...
                {SNodes,[Node|NoSNodes]};
            [] ->
                {SNodes,[Node|NoSNodes]};
            _ ->
                %% How do we handle already created nodes...
                {[Node|SNodes],NoSNodes}
        end
    end,
    case lists:foldl(F2,{[],[]},Nodes) of
        %% Run once to create all...
        {[],NoSchemaNodes} ->
            %% If it has first run dummy ram schema, delete it.
            lists:foreach(fun(Node) ->
                case rpc:call(Node,mnesia,table_info,[schema,ram_copies]) of
                    [Node] ->
                        stopped = rpc:call(Node,mnesia,stop,[]),
                        mnesia:delete_schema([Node]);
                    [] ->
                        ok
                end
            end, NoSchemaNodes),
            %% Create the schema on all nodes...
            ?INFO("Trying to create schema...\n"),
            try
            case mnesia:create_schema(NoSchemaNodes) of
                ok ->
                    ?INFO("Schema Created...\n"),
                    [ begin
                        ok = rpc:call(N, mnesia, start, [])
                    end || N <- NoSchemaNodes ],
                    [ begin
                        Tbl:create_table(NoSchemaNodes)
                      end || Tbl <- MnesiaTbls ],
                    mnesia:wait_for_tables(MnesiaTbls,infinity);
                {error,{Node,{already_exists,Node}}} ->
                    %% How would this ever happen?
                    ?INFO("Schema issue {error,{_Node,{already_exists,~p}}}...\n",[Node]),
                    erlang:exit(mnesia_schema_issue);
                {error,{RemoteNode,{_,RemoteNode,nodedown}}} ->
                    ?INFO("Schema issue {error,{~p,{_,~p,nodedown}}}...\n",
                        [RemoteNode]),
                    erlang:exit(mnesia_nodes_unavailable)
            end
            catch
                C:E ->
                    ?INFO("~p\n",[{C,E}])
            end;
        {SchemaNodes,NoSchemaNodes} ->
            ?INFO("Some nodes have schema, others not...\n{~p,~p}",[SchemaNodes,NoSchemaNodes]),
            %% Fix the nodes, with no schema....
            %% TODO: create schema on NoSchemaNodes
            lists:map(fun(_NSNode) ->
                ok
                % do something like add extra db node, or something...
            end, NoSchemaNodes),
            Nodes = SchemaNodes ++ NoSchemaNodes,
            %% Here i assume, everyone has the synced schema....
            lists:map(fun(SNode) ->
                case rpc:call(SNode, application, get_application,
                              [mnesia]) of
                    undefined ->
                        %% try because, the other node, could still be starting up.
                        ok = try_start_mnesia(SNode);
                    {ok,mnesia} ->
                        ok
                end
            end, SchemaNodes ++ NoSchemaNodes),
            mnesia:wait_for_tables(MnesiaTbls,infinity),
            mnesia:start()
    end,
    ok = mnesia:set_master_nodes([MasterNode]),
    lists:foreach(fun(N) ->
        rpc:call(N,application,start,[pasture])
    end, Nodes);
%% All the other, non master nodes, just idle...
init_master(_MnesiaTbls,_MasterNode) ->
    %% Here i assume that , if the schema is ready,
    %% we can carry on, and get pasture started,
    %% By starting mnesia...
    try
        TblInfo=mnesia:table_info(schema, all),
        mnesia:start()
    catch
        exit:{aborted,{no_exists,schema,all}} ->
        %% I THINK ...
            mnesia:start()
    end.

try_start_mnesia(Node) ->
    try_start_mnesia(Node,10).

try_start_mnesia(Node,0) ->
    ?INFO("Coudn't start mnesia on ~p\n",[Node]),
    erlang:exit(remote_mnesia_couldnt_start);
try_start_mnesia(Node,Count) ->
    case rpc:call(Node, mnesia, start, []) of
        {error,{not_started,NotStarted}} ->
            ?INFO("Retrying mnesia start, ~p is not started on the remote node...\n",[NotStarted]),
            timer:sleep(50),
            try_start_mnesia(Node,Count-1);
        ok ->
            ok
    end.

wait_for_cluster(Nodes,F) ->
    check_node_status(false,Nodes,F).

check_node_status(true,_,_F) ->
    ?INFO("All nodes ready...\n\n"),
    ok;
check_node_status(false,Nodes,F) ->
    ?INFO("check_node_status ~p...\n\n",[Nodes]),
    timer:sleep(100),
    check_node_status(lists:all(F, Nodes),Nodes,F).

json_to_recs([]) ->
    ok;
json_to_recs([{<<"event">>,Objs}|T]) ->
    Rec = pasture_event:new(Objs),
    ok = pasture_db_batch:add({pasture_event,Rec}),
    json_to_recs(T);
json_to_recs([{<<"group">>,Objs}|T]) ->
    Rec = pasture_group:new(Objs),
    ok = pasture_db_batch:add({pasture_group,Rec}),
    json_to_recs(T);
json_to_recs([{<<"member">>,Objs}|T]) ->
    Rec = pasture_member:new(Objs),
    ok = pasture_db_batch:add({pasture_member,Rec}),
    json_to_recs(T);
json_to_recs([{<<"venue">>,Objs}|T]) ->
    Rec = pasture_venue:new(Objs),
    ok = pasture_db_batch:add({pasture_venue,Rec}),
    json_to_recs(T);
json_to_recs([_H|T]) ->
    json_to_recs(T).
