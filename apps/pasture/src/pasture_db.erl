-module(pasture_db).

-export([json_to_recs/2
         ]).

-include("../include/pasture.hrl").

json_to_recs(_Mod, []) ->
    ok;
json_to_recs(Mod, [{<<"event">>,Objs}|T]) ->
    Rec = pasture_event:new(Objs),
    ok = Mod:add(Rec),
    json_to_recs(Mod, T);
json_to_recs(Mod, [{<<"group">>,Objs}|T]) ->
    Rec = pasture_group:new(Objs),
    ok = Mod:add(Rec),
    json_to_recs(Mod, T);
json_to_recs(Mod, [{<<"member">>,Objs}|T]) ->
    Rec = pasture_member:new(Objs),
    ok = Mod:add(Rec),
    json_to_recs(Mod, T);
json_to_recs(Mod, [{<<"venue">>,Objs}|T]) ->
    Rec = pasture_venue:new(Objs),
    ok = Mod:add(Rec),
    json_to_recs(Mod, T);
json_to_recs(Mod, [_H|T]) ->
    json_to_recs(Mod, T).
