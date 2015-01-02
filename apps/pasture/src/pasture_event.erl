-module (pasture_event).

-export([create_table/1,
         new/1]).

-include("../include/pasture.hrl").

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

new(Objs) ->
    Rec =
        #pasture_event{
            event_id   = pasture_utils:try_get_column(Objs,<<"event_id">>),
            event_name = pasture_utils:try_get_column(Objs,<<"event_name">>),
            event_url  = pasture_utils:try_get_column(Objs,<<"event_url">>),
            time       = pasture_utils:try_get_column(Objs,<<"time">>)
        },
    dirty(Rec),
    {atomic,ok}.

new_trx(Objs) ->
    Rec =
        #pasture_event{
            event_id   = pasture_utils:try_get_column(Objs,<<"event_id">>),
            event_name = pasture_utils:try_get_column(Objs,<<"event_name">>),
            event_url  = pasture_utils:try_get_column(Objs,<<"event_url">>),
            time       = pasture_utils:try_get_column(Objs,<<"time">>)
        },
    trx(Rec),
    {atomic,ok}.

dirty(Rec) ->
    ok = mnesia:dirty_write(Rec).

trx(Rec) ->
    ok = mnesia:write(Rec).