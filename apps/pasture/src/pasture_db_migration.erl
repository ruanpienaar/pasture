-module(pasture_db_migration).

-export([ migrate/0 ]).

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

migrate() ->
    Nodes = application:get_env(pasture, mnesia_nodes, [node()]),
	Tables = [pasture_event,pasture_group,pasture_venue,pasture_member],

    %% Copy, and delete original...
    ok = bck_tables(Nodes,Tables),
    {atomic,ok} = mnesia:transaction( fun() ->
        transfer_entries(Tables)
    end ),
    ok = del_tables(Tables),

    %% Create new table spec, and restore
    %% transformed data.
    ok = create_new_migrated_tbls(Nodes,Tables),

    {atomic,ok} = mnesia:transaction( fun() ->
        transform_data_and_restore(Tables)
    end ),

    %% Remove Bck Tables...
    ok = del_bck_tables(Tables),

    ok.

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
    ok = copy_tbl(Tbl,BckTbl,mnesia:first(Tbl)),
    transfer_entries(T).

copy_tbl(_Tbl,_BckTbl,'$end_of_table') ->
    ok;
copy_tbl(Tbl,BckTbl,Id) ->
    [Rec] = mnesia:read(Tbl, Id),
    RecList = tuple_to_list(Rec),
    BckRec = list_to_tuple([BckTbl] ++ lists:nthtail(1,RecList)),
    ok = mnesia:write(BckRec),
    copy_tbl(Tbl,BckTbl,mnesia:next(Tbl,Id)).

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
    ok = restore_tbl(H,BckTbl,mnesia:first(BckTbl)),
    transform_data_and_restore(T).

restore_tbl(_OrigTbl,_BckTbl,'$end_of_table') ->
    ok;
restore_tbl(OrigTbl,BckTbl,Id) ->
    [Rec] = mnesia:read(BckTbl, Id),
    Transform = transform_entry(OrigTbl,Rec),
    ok = mnesia:write(Transform),
    restore_tbl(OrigTbl,BckTbl,mnesia:next(BckTbl,Id)).

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


%% -----
%% Misc

bck_name(Tbl) ->
    list_to_atom(atom_to_list(Tbl)++"_bck").