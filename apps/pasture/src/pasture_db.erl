-module(pasture_db).

-export([init/0,
         json_to_recs/1
         ]).

-include("../include/pasture.hrl").

init() ->
    Master = application:get_env(pasture, mnesia_master, node()),
    case Master =:= node() of
        true ->
            Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
            lager:info("ALive nodes() : ~p\n",[nodes()]),
            Nodes = test_nodes(Nodes,Nodes,20),
            lager:info("Trying to install schema on ~p\n",[Nodes]),
            stopped = mnesia:stop(),
            ok = mnesia:set_master_nodes([Master]),
            case mnesia:create_schema(lists:reverse(Nodes)) of
                ok ->
                    [ rpc:call(N, mnesia, start, []) || N <- Nodes ];
                {error,{_Node,{already_exists,_Node}}} ->
                    [ rpc:call(N, mnesia, start, []) || N <- Nodes ];
                {error,{RemoteNode,{_,RemoteNode,nodedown}}} ->
                    throw(stop)
            end,
            MnesiaTbls=
                [pasture_event,pasture_group,pasture_venue,pasture_member],

            %% Id's
            pasture_ids:create_table(Nodes),

            [ begin
                Tbl:create_table(Nodes),
                pasture_ids:create_id(Tbl)
              end || Tbl <- MnesiaTbls ],
            ?INFO("Waiting for tables...\n"),
            ok = mnesia:wait_for_tables(MnesiaTbls,infinity);
            %% TODO: wait for rest
        false ->
            ?INFO("Not master, just booting...\n")
    end.

test_nodes(_Nodes,[],0) ->
    erlang:exit(mnesia_nodes_unavailable);
test_nodes(Nodes,[],_Count) ->
    Nodes;
test_nodes(Nodes,[H|T],Count) ->
    case net_adm:ping(H) of
        pong ->
            test_nodes(Nodes,T,Count);
        pang ->
            timer:sleep(500),
            lager:info("Waiting for nodes to become alive...",[]),
            test_nodes(Nodes,Nodes,Count-1)
    end.

json_to_recs([]) ->
    ok;
json_to_recs([{<<"event">>,Objs}|T]) ->
    ok = pasture_event:new(Objs),
    json_to_recs(T);
json_to_recs([{<<"group">>,Objs}|T]) ->
    ok = pasture_group:new(Objs),
    json_to_recs(T);
json_to_recs([{<<"member">>,Objs}|T]) ->
    ok = pasture_member:new(Objs),
    json_to_recs(T);
json_to_recs([{<<"venue">>,Objs}|T]) ->
    ok = pasture_venue:new(Objs),
    json_to_recs(T);
json_to_recs([_H|T]) ->
    json_to_recs(T).
