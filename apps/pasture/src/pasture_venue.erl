-module (pasture_venue).

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
        #pasture_venue{
            lat        = pasture_utils:try_get_column(Objs,<<"lat">>),
            lon        = pasture_utils:try_get_column(Objs,<<"lon">>),
            venue_id   = pasture_utils:try_get_column(Objs,<<"venue_id">>),
            venue_name = pasture_utils:try_get_column(Objs,<<"venue_name">>)
        }.