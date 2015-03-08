-module (pasture_twitter_search).

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
                         {attributes,record_info(fields, pasture_event)},
                         {majority, true}
                       ]);
        C:E ->
            {error,{C,E}}
    end.

new([SS,CD]) ->
        #pasture_twitter_search{
            search_str = SS,
            created_datetime = CD
        }.