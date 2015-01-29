-module (pasture_group).

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
                         {attributes,record_info(fields, ?MODULE)},
                         {majority, true}
                       ]);
        C:E ->
            throw({stop,[{c,C},{e,E}]})
    end.

new(Objs) ->
        #pasture_group{
            group_id      = pasture_utils:try_get_column(
                                Objs,<<"group_id">>),
            group_city    = pasture_utils:try_get_column(
                                Objs,<<"group_city">>),
            group_country = pasture_utils:try_get_column(
                                Objs,<<"group_country">>),
            group_lat     = pasture_utils:try_get_column(
                                Objs,<<"group_lat">>),
            group_lon     = pasture_utils:try_get_column(
                                Objs,<<"group_lon">>),
            group_name    = pasture_utils:try_get_column(
                                Objs,<<"group_name">>),
            group_state   = pasture_utils:try_get_column(
                                Objs,<<"group_state">>),
            group_topics  = pasture_utils:decode_topics(
                                pasture_utils:try_get_column(
                                    Objs,<<"group_topics">>)),
            group_urlname = pasture_utils:try_get_column(
                                Objs,<<"group_urlname">>)
        }.