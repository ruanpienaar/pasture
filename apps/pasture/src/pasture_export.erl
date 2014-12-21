
-module (pasture_export).
-export ([start/0
         ]).
-include("../include/pasture.hrl").

start() ->
    json_to_record(start).

json_to_record(start) ->
    do_json_to_rec(get_json(start));
json_to_record('$end_of_table') ->
    ok;
json_to_record(PrevJson) ->
    do_json_to_rec(get_json(PrevJson)).

do_json_to_rec(Json) ->
    Recs = parse_json_to_recs(Json,[]),
    lists:foreach(fun(Rec) ->
        {atomic,ok} = mnesia:dirty_write(Rec)
    end,Recs),
    json_to_record(Json).

get_json(start) ->
    mnesia:transaction( fun() -> mnesia:first() end );
get_json(Prev) ->
    mnesia:transaction( fun() -> mnesia:first(pasture_meeup,Prev) end ).

parse_json_to_recs([{<<"event">>,Objs}|T],R) ->
    parse_json_to_recs(T,[#pasture_event{
        event_id    = try_get_column(Objs,<<"event_id">>),
        event_name  = try_get_column(Objs,<<"event_name">>),
        event_url   = try_get_column(Objs,<<"event_url">>),
        time        = try_get_column(Objs,<<"time">>)
    } | R]);
parse_json_to_recs([{<<"group">>,Objs}|T],R) ->
    parse_json_to_recs(T,[#pasture_group{
        group_city      = try_get_column(Objs,<<"group_city">>),
        group_country   = try_get_column(Objs,<<"group_country">>),
        group_id        = try_get_column(Objs,<<"group_id">>),
        group_lat       = try_get_column(Objs,<<"group_lat">>),
        group_lon       = try_get_column(Objs,<<"group_lon">>),
        group_name      = try_get_column(Objs,<<"group_name">>),
        group_state     = try_get_column(Objs,<<"group_state">>),
        group_topics    = try_get_column(Objs,<<"group_topics">>),
        group_urlname   = try_get_column(Objs,<<"group_urlname">>)
    } | R]);
parse_json_to_recs([{<<"member">>,Objs}|T],R) ->
    parse_json_to_recs(T,[#pasture_member{
        member_id       = try_get_column(Objs,<<"member_id">>),
        member_name     = try_get_column(Objs,<<"member_name">>),
        other_services  = try_get_column(Objs,<<"other_services">>),
        photo           = try_get_column(Objs,<<"other_services">>)
    } | R]);
parse_json_to_recs([{<<"venue">>,Objs}|T],R) ->
    parse_json_to_recs(T,[#pasture_venue{
        lat         = try_get_column(Objs,<<"lat">>),
        lon         = try_get_column(Objs,<<"lon">>),
        venue_id    = try_get_column(Objs,<<"venue_id">>),
        venue_name  = try_get_column(Objs,<<"venue_name">>)
    } | R]).

try_get_column(Objs,Search) ->
    case lists:keyfind(Search,1,Objs) of
        %% Todo; some value's could be more json...
        %% decode properly...
        {Search,Value}  -> Value;
        false           -> undefined
    end.