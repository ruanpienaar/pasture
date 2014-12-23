-module (pasture_member).

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
        #pasture_member{
            id             = pasture_ids:inc(?MODULE),
            member_id      = pasture_utils:try_get_column(
                Objs,<<"member_id">>),
            member_name    = pasture_utils:try_get_column(
                Objs,<<"member_name">>),
            other_services = pasture_utils:try_get_column(
                Objs,<<"other_services">>),
            photo          = pasture_utils:try_get_column(
                Objs,<<"other_services">>)
        },
    ok = mnesia:dirty_write(Rec).