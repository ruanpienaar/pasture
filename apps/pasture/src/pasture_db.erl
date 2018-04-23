-module(pasture_db).

-export([
        json_to_recs/2
         ]).

-include("../include/pasture.hrl").

% Deprecated jsx code.
% json_to_recs(_Mod, []) ->
%     ok;
% json_to_recs(Mod, [{<<"event">>,Objs}|T]) ->
%     Rec = pasture_event:new(Objs),
%     ok = Mod:add(Rec),
%     json_to_recs(Mod, T);
% json_to_recs(Mod, [{<<"group">>,Objs}|T]) ->
%     Rec = pasture_group:new(Objs),
%     ok = Mod:add(Rec),
%     json_to_recs(Mod, T);
% json_to_recs(Mod, [{<<"member">>,Objs}|T]) ->
%     Rec = pasture_member:new(Objs),
%     ok = Mod:add(Rec),
%     json_to_recs(Mod, T);
% json_to_recs(Mod, [{<<"venue">>,Objs}|T]) ->
%     Rec = pasture_venue:new(Objs),
%     ok = Mod:add(Rec),
%     json_to_recs(Mod, T);
% json_to_recs(Mod, [_H|T]) ->
%     json_to_recs(Mod, T).

json_to_recs(DbMod, {Json}) ->

    lists:foldl(fun
        ({<<"venue">>,_} = JsonPortion,Acc) ->
            [JsonPortion|Acc];
        ({<<"member">>,_} = JsonPortion,Acc) ->
            [JsonPortion|Acc];
        ({<<"event">>,_} = JsonPortion,Acc) ->
            [JsonPortion|Acc];
        ({<<"group">>,_} = JsonPortion,Acc) ->
            [JsonPortion|Acc];
        (_, Acc) ->
            Acc
    end, [], Json),
    []. %% TODO: still retrun all the valid recors here.

    % Rec = pasture_venue:new(Json),
    % ok = DbMod:add(Rec).

% Notes:
% 1) response, needs a counter. or another lookup table
% 2) same for guests, we need to record that
% 3) rsvp_id ? what was that ?

% {[
%     {<<"venue">>, {[
%         {<<"venue_name">>,<<"Microsoft">>},
%         {<<"lon">>,-73.985207},
%         {<<"lat">>,40.759727},
%         {<<"venue_id">>,18864822}
%     ]}},
%     {<<"visibility">>,<<"public">>},
%     {<<"response">>,<<"yes">>},
%     {<<"guests">>,0},
%     {<<"member">>, {[
%         {<<"member_id">>,220693553},
%         {<<"photo">>,<<"http://photos1.meetupstatic.com/photos/member/c/c/b/1/thumb_265192401.jpeg">>},
%         {<<"member_name">>,<<"LD">>}
%     ]}},
%     {<<"rsvp_id">>,1659330742},
%     {<<"mtime">>,1490129038721},
%     {<<"event">>,{[
%         {<<"event_name">>,<<"Exploring ASP.NET Core with Erik Noren Part 2">>},
%         {<<"event_id">>,<<"zpvzpmywfbtb">>},
%         {<<"time">>,1490221800000},
%         {<<"event_url">>,<<"https://www.meetup.com/nyaltnet/events/234880646/">>}
%     ]}},
%     {<<"group">>,{[
%         {<<"group_topics">>,[
%             {[
%                 {<<"urlkey">>,<<"softwaredev">>},
%                 {<<"topic_name">>,<<"Software Development">>}
%             ]},
%             {[
%                 {<<"urlkey">>,<<"agile-project-management">>},
%                 {<<"topic_name">>,<<"Agile Project Management">>}
%             ]},
%             {[
%                 {<<"urlkey">>,<<"dotnet">>},
%                 {<<"topic_name">>,<<".NET">>}
%             ]}
%         ]},
%         {<<"group_city">>,<<"New York">>},
%         {<<"group_country">>,<<"us">>},
%         {<<"group_id">>,1270359},
%         {<<"group_name">>,<<"New York ALT.NET Software Development Group">>},
%         {<<"group_lon">>,-73.99},
%         {<<"group_urlname">>,<<"nyaltnet">>},
%         {<<"group_state">>,<<"NY">>},
%         {<<"group_lat">>,40.73}
%     ]}}
% ]}
