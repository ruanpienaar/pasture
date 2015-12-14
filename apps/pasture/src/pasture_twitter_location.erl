-module(pasture_twitter_location).

-export([create_table/1,
         new/1,
         inc/1]).

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
                         {ram_copies,Nodes},
                         {attributes,record_info(fields, pasture_twitter_location)},
                         {majority, true}
                       ]);
        C:E ->
            {error,{C,E}}
    end.

new([Location]) ->
    #pasture_twitter_location{
        location = Location
    }.

inc([Location]) ->
    mnesia:transaction(
        fun() ->
            NewLoc = 
                case mnesia:read(?MODULE, Location) of 
                    [] ->
                        #pasture_twitter_location{
                            location = Location
                        };
                    [Loc] ->
                        Loc#pasture_twitter_location{
                            count = Loc#pasture_twitter_location.count + 1
                        }
                end,
            ok = mnesia:write(NewLoc)
        end
    ).