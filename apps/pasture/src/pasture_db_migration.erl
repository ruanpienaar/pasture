-module(pasture_db_migration).

-export([ migrate_1/0,
          migrate_2/0,
          migrate_3/0,
          migrate_4/0,
          migrate_5/0,
          migrate_6/0%,
%          id_check/2
 ]).

%% edit the original record before running migrate....
%% PREVIOUS -> -record(test,{id,col1,col2}).
-record(test,{col1,col2}).
-record(test_bck,{id,col1,col2}).

-include("../include/pasture.hrl").
-record(pasture_event_bck,{id,
                       event_id,
                       event_name,
                       event_url,
                       time
                      }).
-record(pasture_group_bck,{id,
                       group_city,
                       group_country,
                       group_id,
                       group_lat,
                       group_lon,
                       group_name,
                       group_state,
                       group_topics,
                       group_urlname
                      }).
-record(pasture_member_bck,{id,
                        member_id,
                        member_name,
                        other_services,
                        photo
                       }).
-record(pasture_venue_bck,{id,
                       lat,
                       lon,
                       venue_id,
                       venue_name
                      }).

migrate_1() ->
    Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
	Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    %% Copy, and delete original...
    ok = bck_tables(Nodes,Tables).

migrate_2() ->
    %% This can't all be in 1 trx
    Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    ok = transfer_entries(Tables).

migrate_3() ->
    Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    ok = del_tables(Tables).

migrate_4() ->
    %% Create new table spec
    Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
    Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    ok = create_new_migrated_tbls(Nodes,Tables).

migrate_5() ->
    %% restore transformed data.
    Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    transform_data_and_restore(Tables).

migrate_6() ->
    Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],
    %% Remove Bck Tables...
    del_bck_tables(Tables).

%% -----------------------

bck_tables(_Nodes,[]) ->
    ok;
bck_tables(Nodes,[test|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            test_bck,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the OLD SPEC here
             {attributes,record_info(fields, test_bck)}]
        ),
    bck_tables(Nodes,T);
bck_tables(Nodes,[pasture_event|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_event_bck,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the OLD SPEC here
             {attributes,record_info(fields, pasture_event_bck)}]
        ),
    bck_tables(Nodes,T);
bck_tables(Nodes,[pasture_group|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_group_bck,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the OLD SPEC here
             {attributes,record_info(fields, pasture_group_bck)}]
        ),
    bck_tables(Nodes,T);
bck_tables(Nodes,[pasture_venue|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_venue_bck,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the OLD SPEC here
             {attributes,record_info(fields, pasture_venue_bck)}]
        ),
    bck_tables(Nodes,T);
bck_tables(Nodes,[pasture_member|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_member_bck,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the OLD SPEC here
             {attributes,record_info(fields, pasture_member_bck)}]
        ),
    bck_tables(Nodes,T);
bck_tables(Nodes,[_H|T]) ->
    bck_tables(Nodes,T).

transfer_entries([]) ->
    ok;
transfer_entries([Tbl|T]) ->
    BckTbl = bck_name(Tbl),
    ok = copy_tbl(Tbl,BckTbl,mnesia:dirty_first(Tbl),0,[]),
    transfer_entries(T).

copy_tbl(_Tbl,_BckTbl,'$end_of_table',_,[]) ->
    ok;
copy_tbl(_Tbl,_BckTbl,'$end_of_table',_,Stack) ->
    commit_stack(Stack);
copy_tbl(Tbl,BckTbl,Id,500,Stack) ->
    ok = commit_stack(Stack),
    copy_tbl(Tbl,BckTbl,Id,0,[]);
copy_tbl(Tbl,BckTbl,Id,Count,Stack) ->
    [Rec] = mnesia:dirty_read(Tbl, Id),
    RecList = tuple_to_list(Rec),
    BckRec = list_to_tuple([BckTbl] ++ lists:nthtail(1,RecList)),
    copy_tbl(Tbl,BckTbl,mnesia:dirty_next(Tbl,Id),Count+1,[BckRec|Stack]).

commit_stack(Stack) ->
    {atomic,ok} = mnesia:transaction( fun() ->
        lists:foreach(fun(Rec) ->
            ok = mnesia:write(Rec)
        end, Stack)
    end),
    ok.

del_tables([]) ->
    ok;
del_tables([H|T]) ->
    {atomic,ok} = mnesia:delete_table(H),
    del_tables(T).

%% -----------------------

create_new_migrated_tbls(_Nodes,[]) ->
    ok;
create_new_migrated_tbls(Nodes,[test|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            test,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the NEW SPEC here
             {attributes,record_info(fields, test)}]
        ),
    create_new_migrated_tbls(Nodes,T);
create_new_migrated_tbls(Nodes,[pasture_event|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_event,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the NEW SPEC here
             {attributes,record_info(fields, pasture_event)}]
        ),
    create_new_migrated_tbls(Nodes,T);
create_new_migrated_tbls(Nodes,[pasture_group|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_group,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the NEW SPEC here
             {attributes,record_info(fields, pasture_group)}]
        ),
    create_new_migrated_tbls(Nodes,T);
create_new_migrated_tbls(Nodes,[pasture_venue|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_venue,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the NEW SPEC here
             {attributes,record_info(fields, pasture_venue)}]
        ),
    create_new_migrated_tbls(Nodes,T);
create_new_migrated_tbls(Nodes,[pasture_member|T]) ->
    {atomic,ok} =
        mnesia:create_table(
            pasture_member,
            [{type,set},
             {disc_only_copies,Nodes},
             %% NOTE: use the NEW SPEC here
             {attributes,record_info(fields, pasture_member)}]
        ),
    create_new_migrated_tbls(Nodes,T);
create_new_migrated_tbls(Nodes,[_H|T]) ->
    create_new_migrated_tbls(Nodes,T).

transform_data_and_restore([]) ->
    ok;
transform_data_and_restore([H|T]) ->
    BckTbl = bck_name(H),
    ok = restore_tbl(H,BckTbl,mnesia:dirty_first(BckTbl),0,[]),
    transform_data_and_restore(T).

% restore_tbl(_OrigTbl,_BckTbl,'$end_of_table') ->
%     ok;
% restore_tbl(OrigTbl,BckTbl,Id) ->
%     [Rec] = mnesia:dirty_read(BckTbl, Id),
%     Transform = transform_entry(OrigTbl,Rec),
%     %% ok = mnesia:write(Transform),
%     restore_tbl(OrigTbl,BckTbl,mnesia:next(BckTbl,Id)).

restore_tbl(_Tbl,_BckTbl,'$end_of_table',_,[]) ->
    ok;
restore_tbl(_Tbl,_BckTbl,'$end_of_table',_,Stack) ->
    commit_stack(Stack);
restore_tbl(Tbl,BckTbl,Id,500,Stack) ->
    ok = commit_stack(Stack),
    restore_tbl(Tbl,BckTbl,Id,0,[]);
restore_tbl(Tbl,BckTbl,Id,Count,Stack) ->
    [Rec] = mnesia:dirty_read(BckTbl, Id),
    NewRec = transform_entry(Tbl,Rec),
    restore_tbl(Tbl,BckTbl,mnesia:dirty_next(BckTbl,Id),
                Count+1,[NewRec|Stack]).

transform_entry(test,{test_bck,_Id,V1,V2}) ->
    #test{col1=V1,col2=V2};
transform_entry(pasture_event,{pasture_event_bck,_A1,A2,A3,A4,A5}) ->
    #pasture_event{event_id = A2,
                   event_name = A3,
                   event_url = A4,
                   time = A5};
transform_entry(pasture_group,{pasture_group_bck,_A1,A2,A3,A4,A5,
                                                A6,A7,A8,A9,A10}) ->
    #pasture_group{group_id = A4,
                   group_city = A2,
                   group_country = A3,
                   group_lat = A5,
                   group_lon = A6,
                   group_name = A7,
                   group_state = A8,
                   group_topics = A9,
                   group_urlname = A10};
transform_entry(pasture_member,{pasture_member_bck,_A1,A2,A3,A4,A5}) ->
    #pasture_member{member_id = A2,
                    member_name = A3,
                    other_services = A4,
                    photo = A5};
transform_entry(pasture_venue,{pasture_venue_bck,_A1,A2,A3,A4,A5}) ->
    #pasture_venue{venue_id = A4,
                   lat = A2,
                   lon = A3,
                   venue_name = A5}.

del_bck_tables([]) ->
    ok;
del_bck_tables([H|T]) ->
    {atomic,ok} = mnesia:delete_table(bck_name(H)),
    del_bck_tables(T).

% id_check(pasture_event,Id) ->
%     mnesia:


%% -----
%% Misc

bck_name(Tbl) ->
    list_to_atom(atom_to_list(Tbl)++"_bck").

