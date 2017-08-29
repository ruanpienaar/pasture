-module(pasture_db_esqlite).


-behaviour(gen_server).
-behaviour(pasture_db_mod).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add/1
         ,new_batch_size/1
         ,commit/1
]).

-include("pasture.hrl").

-define(STATE, pasture_db_esqlite_state).
-record(?STATE, { bs,
                  dbc,
                  statements = [],
                  acc = 0,
                  acc_list=[],
                  busy=false,
                  workers=[]
                }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

add(Obj) ->
    try
        gen_server:cast(?MODULE,{add, Obj})
    catch
        C:E ->
            ?EMERGENCY("catch : ~p ~p ~p", [C,E,erlang:get_stacktrace()])
    end.

init({}) ->
    erlang:send_after(5000, self(), write),
    false = process_flag(trap_exit, true),
    % {ok,BS} = application:get_env(pasture, batch_size),
    BS = 15,
    %% TODO: make a db for every day...
    {ok, DBC} = esqlite3:open(code:priv_dir(pasture)++"/pasture.db"),
    ok = create_tables(DBC),

     {ok, S1} = esqlite3:prepare("insert or replace into pasture_event values(?1, ?2, ?3, ?4);", DBC),
     {ok, S2} = esqlite3:prepare("insert or replace into pasture_group values(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9);", DBC),
     {ok, S3} = esqlite3:prepare("insert or replace into pasture_member values(?1, ?2, ?3, ?4);", DBC),
     {ok, S4} = esqlite3:prepare("insert or replace into pasture_venue values(?1, ?2, ?3, ?4);", DBC),
     {ok, S5} = esqlite3:prepare("insert or replace into pasture_twitter values(?1, ?2, ?3)", DBC),
     {ok, S6} = esqlite3:prepare("insert or replace into pasture_google_trend values(?1, ?2, ?3, ?4, ?5);", DBC),
     {ok, S7} = esqlite3:prepare("insert or replace into pasture_google_trend_news_item values(?1, ?2, ?3, ?4, ?5, ?6);", DBC),

    {ok, #?STATE{
        bs=BS,
        dbc = DBC,
        statements = [
            {pasture_event, S1},
            {pasture_group, S2},
            {pasture_member, S3},
            {pasture_venue, S4},
            {pasture_twitter, S5},
            {pasture_google_trend, S6},
            {pasture_google_trend_news_item, S7}
        ]
    }}.

% handle_call(Obj, _From, #?STATE{bs = _BS, b = B, dbc=DBC} = State) when B==0 ->
%     ok = do_begin(DBC),
%     S = statement(State, Obj),
%     ok = insert(S,Obj),
%     {reply, ok, State#?STATE{b=1}};
% handle_call(Obj, _From, #?STATE{bs = BS, b = B, dbc=DBC} = State) when B>=BS ->
%     S = statement(State, Obj),
%     ok = insert(S,Obj),
%     ok = commit(DBC),
%     {reply, ok, State#?STATE{b=0}};
% handle_call(Obj, _From, #?STATE{bs = BS, b = B, dbc=DBC} = State) when B<BS->
%     S = statement(State, Obj),
%     ok = insert(S,Obj),
%     {reply, ok, State#?STATE{b=B+1}}.
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({add, Obj}, #?STATE{bs = BS, acc=Acc,
                                statements = Statements} = State) when Acc >= BS ->
    {noreply, busy_state(State, worker(Statements,[Obj|State#?STATE.acc_list]))};
handle_cast({add, Obj}, #?STATE{bs = BS,acc=Acc} = State) when Acc < BS ->
    {noreply, State#?STATE{ acc = Acc+1, acc_list = [Obj|State#?STATE.acc_list] }};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(write, #?STATE{ acc_list = [] } = State) ->
    erlang:send_after(5000, self(), write),
    {noreply, State};
handle_info(write, #?STATE{ acc_list = AL,
                            statements = Statements} = State) when AL /= [] ->
    erlang:send_after(5000, self(), write),
    {noreply, busy_state(State, worker(Statements, AL))};
handle_info(write, State) ->
    erlang:send_after(5000, self(), write),
    {noreply, State};
% handle_info({'EXIT', W, normal}, #?STATE{ workers = Workers, busy = true } = State) ->
%     %?INFO("'EXIT' ~p ", [W]),
%     case Workers--[W] of
%         Workers ->
%             ?EMERGENCY("Unknown worker died...", []),
%             {noreply, State#?STATE{ busy = false }};
%         [] ->
%             ?CRITICAL("No More Workers alive...", []),
%             {noreply, State#?STATE{ busy = false, workers = [] }};
%         RestWorkers ->
%             ?CRITICAL("Rest Workers: ~p...", [RestWorkers]),
%             {noreply, State#?STATE{ workers = RestWorkers }}
%     end;
handle_info(Info, State) ->
    ?EMERGENCY("~p Info ~p Workers ~p~n", [?MODULE, Info, State#?STATE.workers]),
    {noreply, State}.

worker(Statements, L) ->
    % W=
    % proc_lib:spawn_link(fun() ->
        % put(pasture_db_esqlite_worker_pid, self()),
        lists:foreach(fun(I) ->
            '$done' = insert(statement(Statements, element(1, I)), I)
        end, L).
	%% ,
        %%?INFO("[~p] ~p Inserted ~p entries...",[?MODULE, self(), length(L)]).
    % end),
    % ?WARNING("Spawned worker: ~p~n", [W]),
    % W.

statement(Statements, ObjAtom) ->
    {ObjAtom,Statement} = lists:keyfind(ObjAtom, 1, Statements),
    Statement.

busy_state(State, W) ->
    State#?STATE{ acc = 0, acc_list = [] }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%----------------

new_batch_size(_Size) ->
    ok.

% do_begin(_DBC) ->
    % ok = esqlite3:exec("begin transaction;", DBC).
%    ok.



insert(Statement,#pasture_event{ event_id=EI,
                           event_name=EN,
                           event_url=EU,
                           time=T } = _Rec) ->
    ok = esqlite3:bind(Statement, [EI,EN,EU,T]),
    esqlite3:step(Statement);
insert(Statement,#pasture_group{ group_id=GID,
                           group_city=GCI,
                           group_country=GCO,
                           group_lat=GLA,
                           group_lon=GLO,
                           group_name=GN,
                           group_state=GS,
                           group_topics=GT,
                           group_urlname=GU } = _Rec) ->
    ok = esqlite3:bind(Statement, [GID, GCI, GCO, GLA, GLO, GN, GS, group_topics_str(GT), GU]),
    esqlite3:step(Statement);
insert(Statement,#pasture_member{ member_id=MI,
                            member_name=MN,
                            other_services=OS,
                            photo=P } = _Rec) ->
    ok = esqlite3:bind(Statement, [MI,MN,other_services(OS),P]),
    esqlite3:step(Statement);
insert(Statement,#pasture_venue{ venue_id=VI,
                           venue_name=VN,
                           lat=LA,
                           lon=LO} = _Rec) ->
    ok = esqlite3:bind(Statement, [VI,VN,LA,LO]),
    esqlite3:step(Statement);
insert(Statement,#pasture_twitter{id=ID,filter_str=Str,json=JSON}) ->
    ok = esqlite3:bind(Statement, [ID,Str,JSON]),
    esqlite3:step(Statement);
insert(Statement, #pasture_google_trend{
                    country_id = CI,
                    title = T,
                    approx_traffic = AT,
                    pub_date = PD,
                    picture_url = PU}) ->
    ok = esqlite3:bind(Statement, [CI,T,AT,PD,PU]),
    esqlite3:step(Statement);
insert(Statement, #pasture_google_trend_news_item{
                    country_id = CI,
                    title = T,
                    pub_date = PD,
                    news_item_title = NT,
                    news_item_snippet = NSN,
                    news_item_source = NSO}) ->
    ok = esqlite3:bind(Statement, [CI,T,PD,NT,NSN,NSO]),
    esqlite3:step(Statement).

commit(_DBC) ->
%     % ok = esqlite3:exec("commit;", DBC).
     ok.

%%----------------

create_tables(Context) ->
    ok = esqlite3:exec("create table if not exists pasture_event (event_id, event_name, event_url, time, PRIMARY KEY(event_id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_group (group_id, group_city, group_country, group_lat, group_lon, group_name, group_state, group_topics, group_urlname, PRIMARY KEY(group_id ASC))", Context),
    ok = esqlite3:exec("create table if not exists pasture_member (member_id, member_name, other_services, photo, PRIMARY KEY(member_id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_venue (venue_id, venue_name, lat, lon, PRIMARY KEY(venue_id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_twitter (id, filter_str, json JSON, PRIMARY KEY(id ASC));", Context),
    ok = esqlite3:exec("create table if not exists pasture_google_trend (country_id INT, title TEXT, approx_traffic TEXT, pub_date TEXT, picture_url TEXT, PRIMARY KEY (country_id, title, pub_date));", Context),
    ok = esqlite3:exec("create table if not exists pasture_google_trend_news_item (country_id INT, title TEXT, pub_date TEXT, news_item1_title TEXT, news_item1_snippet TEXT, news_item1_source TEXT, PRIMARY KEY (country_id, title, pub_date));", Context).

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
other_services([{Service, [{<<"identifier">>, Value}]} | T], R) ->
    other_services(T, [ lists:reverse(binary_to_list(Service)++"="++binary_to_list(Value)) |R]).
