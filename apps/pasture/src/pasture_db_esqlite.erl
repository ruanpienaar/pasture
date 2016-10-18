-module(pasture_db_esqlite).


-behaviour(gen_server).
-behaviour(pasture_db_mod).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add/1,
         new_batch_size/1,
         commit/1
]).

-include("pasture.hrl").

-define(STATE, pasture_db_esqlite_state).
-record(?STATE, { bs,
                  b=0,
                  dbc }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    false = process_flag(trap_exit, true),
    {ok,BS} = application:get_env(pasture, batch_size),
    %% TODO: make a db for every day...
    {ok, Context} = esqlite3:open(code:priv_dir(pasture)++"/pasture.db"),
    ok = create_tables(Context),
    {ok, #?STATE{ bs=BS,
                  dbc = Context }}.

handle_call(Obj, _From, #?STATE{bs = _BS, b = B, dbc=DBC} = State) when B==0 ->
    ok = do_begin(DBC),
    ok = insert(DBC,Obj),
    {reply, ok, State#?STATE{b=1}};
handle_call(Obj, _From, #?STATE{bs = BS, b = B, dbc=DBC} = State) when B>=BS ->
    ok = insert(DBC,Obj),
    ok = commit(DBC),
    {reply, ok, State#?STATE{b=0}};
handle_call(Obj, _From, #?STATE{bs = BS, b = B, dbc=DBC} = State) when B<BS->
    ok = insert(DBC,Obj),
    {reply, ok, State#?STATE{b=B+1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("~p Info ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%----------------

add({_Type,Obj}) ->
    %io:format("S", []),
    gen_server:call(?MODULE,Obj,infinity).

new_batch_size(_Size) ->
    ok.

do_begin(_DBC) ->
    % ok = esqlite3:exec("begin transaction;", DBC).
    ok.

insert(DBC,#pasture_event{ event_id=EI,
                           event_name=EN,
                           event_url=EU,
                           time=T } = _Rec) ->
    {ok, Statement} = esqlite3:prepare("insert or replace into pasture_event values(?1, ?2, ?3, ?4)", DBC),
    ok = esqlite3:bind(Statement, [EI,EN,EU,T]),
    io:format("~p~n", [[EI,EN,EU,T]]),
    _A = esqlite3:step(Statement),
    % io:format("A~p",[A]),
    ok;
insert(DBC,#pasture_group{ group_id=GID,
                           group_city=GCI,
                           group_country=GCO,
                           group_lat=GLA,
                           group_lon=GLO,
                           group_name=GN,
                           group_state=GS,
                           group_topics=GT,
                           group_urlname=GU } = _Rec) ->
    {ok, Statement} = esqlite3:prepare("insert or replace into pasture_group values(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9);", DBC),
    ok = esqlite3:bind(Statement, [GID, GCI, GCO, GLA, GLO, GN, GS, group_topics_str(GT), GU]),
    io:format("~p~n", [[GID, GCI, GCO, GLA, GLO, GN, GS, group_topics_str(GT), GU]]),
    _A = esqlite3:step(Statement),
    % io:format("A~p",[A]),
    ok;
insert(DBC,#pasture_member{ member_id=MI,
                            member_name=MN,
                            other_services=OS,
                            photo=P } = _Rec) ->
    {ok, Statement} = esqlite3:prepare("insert or replace into pasture_member values(?1, ?2, ?3, ?4)", DBC),
    ok = esqlite3:bind(Statement, [MI,MN,other_services(OS),P]),
    io:format("~p~n", [[MI,MN,other_services(OS),P]]),
    _A = esqlite3:step(Statement),
    % io:format("A~p",[A]),
    ok;
insert(DBC,#pasture_venue{ venue_id=VI,
                           venue_name=VN,
                           lat=LA,
                           lon=LO} = _Rec) ->
    {ok, Statement} = esqlite3:prepare("insert or replace into pasture_venue values(?1, ?2, ?3, ?4)", DBC),
    ok = esqlite3:bind(Statement, [VI,VN,LA,LO]),
    io:format("~p~n", [[VI,VN,LA,LO]]),
    _A = esqlite3:step(Statement),
    % io:format("A~p",[A]),
    ok.

commit(_DBC) ->
    % ok = esqlite3:exec("commit;", DBC).
    ok.

%%----------------

create_tables(Context) ->
    ok = esqlite3:exec("create table if not exists pasture_event (event_id, event_name, event_url, time, PRIMARY KEY(event_id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_group (group_id, group_city, group_country, group_lat, group_lon, group_name, group_state, group_topics, group_urlname, PRIMARY KEY(group_id ASC))", Context),
    ok = esqlite3:exec("create table if not exists pasture_member (member_id, member_name, other_services, photo, PRIMARY KEY(member_id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_venue (venue_id, venue_name, lat, lon, PRIMARY KEY(venue_id ASC));", Context).

group_topics_str(TopicList) ->

    group_topics_str(TopicList, []).

group_topics_str([], R) ->
    list_to_binary(lists:reverse(R));
group_topics_str([{urlkey,BinStr}|T], R) ->
    group_topics_str(T, ["|"++lists:reverse(binary_to_list(BinStr))|R]).

other_services(S) ->
    other_services(S, []).

other_services([], R) ->
    lists:reverse(R);
other_services(undefined,[]) ->
    "";
other_services([{<<"facebook">>,[{<<"identifier">>,FbLink}]} | T], R) ->
    other_services(T, ["|"++lists:reverse("facebook="++FbLink)]);
other_services([{<<"twitter">>, [{<<"identifier">>,TwitterID}]} | T], R) ->
    other_services(T, ["|"++lists:reverse("twitter="++TwitterID)|R]).