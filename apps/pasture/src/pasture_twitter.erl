-module(pasture_twitter).

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
                         {attributes,record_info(fields, pasture_twitter)},
                         {majority, true}
                       ]);
        C:E ->
            {error,{C,E}}
    end.

new([Id,FilterStr,Json]) ->
        #pasture_twitter{
            id = Id,
            filter_str = FilterStr,
            json = Json
        }.