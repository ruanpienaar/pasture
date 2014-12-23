-module(pasture_export).
-export([start/0]).
-include("../include/pasture.hrl").

start() ->
    % A1 = mnesia:table_info(pasture_event,size),
    % A2 = mnesia:table_info(pasture_group,size),
    % A3 = mnesia:table_info(pasture_member,size),
    % A4 = mnesia:table_info(pasture_venue,size),
    % json_to_record(start,[A1,A2,A3,A4]).
    ok.

% json_to_record(start,IDS) ->
%     do_json_to_rec(get_json(start),IDS);
% json_to_record(PrevJson,IDS) ->
%     do_json_to_rec(get_json(PrevJson),IDS).

% do_json_to_rec('$end_of_table',_IDS) ->
%     ok;
% do_json_to_rec(Json,IDS) ->
%     {Recs,NewIDS} = parse_json_to_recs(Json,[],IDS),
%     lists:foreach(fun(Rec) ->
%         ok =
%             mnesia:dirty_write(Rec)
%     end,Recs),
%     ok = mnesia:dirty_delete(pasture_meetup,Json),
%     json_to_record(Json,NewIDS).

% get_json(start) ->
%     {atomic,V} =
%         mnesia:transaction( fun() -> mnesia:first(pasture_meetup) end ),
%     V;
% get_json(Prev) ->
%     {atomic,V} =
%         mnesia:transaction( fun() -> mnesia:next(pasture_meetup,Prev) end ),
%     V.

% parse_json_to_recs([], R, IDs) ->
%     {R,IDs};
% parse_json_to_recs([{<<"event">>,Objs}|T],R,[A1,A2,A3,A4]) ->
%     parse_json_to_recs(T,[#pasture_event{
%         id          = A1 + 1,
%         event_id    = try_get_column(Objs,<<"event_id">>),
%         event_name  = try_get_column(Objs,<<"event_name">>),
%         event_url   = try_get_column(Objs,<<"event_url">>),
%         time        = try_get_column(Objs,<<"time">>)
%     } | R], [A1+1,A2,A3,A4]);
% parse_json_to_recs([{<<"group">>,Objs}|T],R,[A1,A2,A3,A4]) ->
%     parse_json_to_recs(T,[#pasture_group{
%         id              = A2 + 1,
%         group_city      = try_get_column(Objs,<<"group_city">>),
%         group_country   = try_get_column(Objs,<<"group_country">>),
%         group_id        = try_get_column(Objs,<<"group_id">>),
%         group_lat       = try_get_column(Objs,<<"group_lat">>),
%         group_lon       = try_get_column(Objs,<<"group_lon">>),
%         group_name      = try_get_column(Objs,<<"group_name">>),
%         group_state     = try_get_column(Objs,<<"group_state">>),
%         group_topics    = decode_topics(
%                             try_get_column(Objs,<<"group_topics">>)),
%         group_urlname   = try_get_column(Objs,<<"group_urlname">>)
%     } | R], [A1,A2+1,A3,A4]);
% parse_json_to_recs([{<<"member">>,Objs}|T],R,[A1,A2,A3,A4]) ->
%     parse_json_to_recs(T,[#pasture_member{
%         id              = A3 + 1,
%         member_id       = try_get_column(Objs,<<"member_id">>),
%         member_name     = try_get_column(Objs,<<"member_name">>),
%         other_services  = try_get_column(Objs,<<"other_services">>),
%         photo           = try_get_column(Objs,<<"other_services">>)
%     } | R], [A1,A2,A3+1,A4]);
% parse_json_to_recs([{<<"venue">>,Objs}|T],R,[A1,A2,A3,A4]) ->
%     parse_json_to_recs(T,[#pasture_venue{
%         id          = A4 + 1,
%         lat         = try_get_column(Objs,<<"lat">>),
%         lon         = try_get_column(Objs,<<"lon">>),
%         venue_id    = try_get_column(Objs,<<"venue_id">>),
%         venue_name  = try_get_column(Objs,<<"venue_name">>)
%     } | R], [A1,A2,A3,A4+1]);
% parse_json_to_recs([_H|T],R,K) ->
%     parse_json_to_recs(T,R,K).

% try_get_column(Objs,Search) ->
%     case lists:keyfind(Search,1,Objs) of
%         %% Todo; some value's could be more json...
%         %% decode properly...
%         {Search,Value}  -> Value;
%         false           -> undefined
%     end.

% decode_topics(undefined) ->
%     undefined;
% decode_topics(V) ->
%     decode_topics(V,[]).

% decode_topics([],R) ->
%     R;
% decode_topics([[{<<"urlkey">>,UrlKey},_]|T],R) ->
%     decode_topics(T,[{urlkey,UrlKey}|R]).