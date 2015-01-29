-module(pasture_db).

-export([init/0,
         json_to_recs/1
         ]).

-include("../include/pasture.hrl").

init() ->
    mnesia:set_debug_level(verbose),
    ?INFO("Db init...\n"),
    {ok,Master} = application:get_env(nodes, master_db_node),
    ?INFO("Master node : ~p\n",[Master]),
    MnesiaTbls=[pasture_event,
                pasture_group,
                pasture_venue,
                pasture_member],
    init(MnesiaTbls,Master).

init(MnesiaTbls,MasterNode) when MasterNode == node() ->
    {ok,Nodes} = application:get_env(nodes, db_nodes),
    [ExtraNodes] = [ Nodes -- [node()] ],
    ?INFO("Extra nodes : ~p\n\n",[ExtraNodes]),
    mnesia:change_config(extra_db_nodes, ExtraNodes),
    stopped = mnesia:stop(),
    ?INFO("Trying to install schema on ~p\n\n",[Nodes]),

    timer:sleep(500),
    case mnesia:create_schema(Nodes) of
        ok ->
            lists:foreach(fun(MN) ->
                rpc:call(MN, mnesia, start, [])
            end, Nodes);
        {error,{_,{already_exists,_}}} ->
            ok
    end,

    lists:foreach(fun(RN) ->
        case rpc:call(RN, application, start, [mnesia]) of
            ok ->
                ok;
            {error,{already_started,mnesia}} ->
                ok
        end
    end, Nodes),
    lists:foreach(fun(Tbl) ->
        Tbl:create_table([MasterNode]),
        lists:foreach(fun(EN) ->
            mnesia:add_table_copy(Tbl, EN, disc_only_copies)
        end, ExtraNodes)
    end, MnesiaTbls),
    mnesia:wait_for_tables(MnesiaTbls, infinity),
    ok;
init(_MnesiaTbls,_MasterNode) ->
    %% TODO: how do you properly know when mnesia has created the schema...?
    case mnesia:system_info(use_dir) of
        true ->
            %% Maybe also subs and pause ?
            ok;
        false ->
            % Subscribe to mnesia system, and wait until it's up....
            nodes_watchdog:subscribe_and_pause(),
            ok
    end.

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
