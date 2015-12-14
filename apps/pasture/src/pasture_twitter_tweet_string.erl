-module (pasture_twitter_tweet_string).

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
                         {ram_copies,Nodes},
                         {attributes,record_info(fields, pasture_twitter_tweet_string)},
                         {majority, true}
                       ]);
        C:E ->
            {error,{C,E}}
    end.

new([Stirng]) ->
        #pasture_twitter_tweet_string{
            string = Stirng
        }.