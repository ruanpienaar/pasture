-module(pasture_db).

-export([init/0,
         json_to_recs/1
         ]).

-include("../include/pasture.hrl").

init() ->
    mnesia:set_debug_level(verbose),
    ?INFO("Db init...\n"),
    {ok,Master} = application:get_env(pasture, master_db_node),
    ?INFO("Master node : ~p\n",[Master]),
    MnesiaTbls=[pasture_event,
                pasture_group,
                pasture_venue,
                pasture_member,
                pasture_twitter,
                pasture_twitter_location],
    init(MnesiaTbls,Master).

init(MnesiaTbls,MasterNode) when MasterNode == node() ->
    {ok,Nodes} = application:get_env(pasture, db_nodes),
    [ExtraNodes] = [ Nodes -- [node()] ],
    ?INFO("Extra nodes : ~p\n\n",[ExtraNodes]),
    mnesia:change_config(extra_db_nodes, ExtraNodes),
    stopped = mnesia:stop(),
    ?INFO("Trying to install schema on ~p\n\n",[Nodes]),
    timer:sleep(2000),
    case mnesia:create_schema(Nodes) of
        ok ->
            ?INFO("Schema created ...\n"),
            ok = mnesia:start();
        {error,{NNN,{already_exists,NNN}}} ->
            ?INFO("Schema already created on ~p ...\n",[NNN]),
            ok = mnesia:start()
    end,
    lists:foreach(fun(RN) ->
        rpc:call(RN, mnesia, start, [])
    end, ExtraNodes),
    lists:foreach(fun(Tbl) ->
        ?INFO("Creating table ~p ...\n\n",[Tbl]),
        Tbl:create_table([MasterNode]),
        lists:foreach(fun(EN) ->
            ?INFO("Add table copy ~p on node ~p ...\n\n",[Tbl, EN]),
            mnesia:add_table_copy(Tbl, EN, disc_only_copies)
        end, ExtraNodes)
    end, MnesiaTbls),
    mnesia:wait_for_tables(MnesiaTbls, infinity),
    ok;
init(_MnesiaTbls,_MasterNode) ->
    stopped = mnesia:stop(),
    ok.

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
