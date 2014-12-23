-module(pasture_ids).

-export([create_table/1,
         create_id/1,
         inc/1]).

-record(?MODULE,{ tbl,
                  id
                }).

create_table(Nodes) ->
    try
        mnesia:table_info(?MODULE,attributes)
    catch
        exit:{aborted,{no_exists,?MODULE,attributes}} ->
            {atomic,ok} =
                mnesia:create_table(
                        ?MODULE,
                        [{type,set},
                         {disc_only_copies,Nodes},
                         {attributes,record_info(fields, ?MODULE)}
                       ]);
        C:E ->
            throw({stop,[{c,C},{e,E}]})
    end.

create_id(Tbl) ->
    {atomic,ok} =
        mnesia:transaction( fun() ->
            mnesia:write(#?MODULE{tbl=Tbl,id=0})
        end).

inc(Tbl) ->
    mnesia:dirty_update_counter(?MODULE, Tbl, 1).