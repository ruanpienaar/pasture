-module(pasture_db_mnesia).

%% TODO: rename to mnesia module....

-behaviour(gen_server).
-behaviour(pasture_db_mod).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add/1,
         new_batch_size/1,
         commit/1
]).

-include("../include/pasture.hrl").

-define(STATE, pasture_db_batch_state).
-record(?STATE,{ bs,
                 pasture_event  = {0,[]},
                 pasture_group  = {0,[]},
                 pasture_member = {0,[]},
                 pasture_venue  = {0,[]},
                 pasture_twitter = {0,[]},
                 m_ready = false
}).

% ---

%% TODO: make the db batch, increase in size, when a node is down,
%% and when mnesia aborts, because the majotiry flag is set to true.

add(Obj) ->
    gen_server:call(?MODULE,Obj,infinity).

new_batch_size(Size) when is_integer(Size) andalso Size > 0 ->
    application:set_env(pasture, batch_size, Size),
    gen_server:cast(?MODULE, {new_batch_size, Size}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%% ----

init({}) ->
    false = process_flag(trap_exit, true),
    {ok,BS} = application:get_env(pasture, batch_size),
    {ok, #?STATE{bs=BS}, 0}.

 handle_call(#pasture_event{} = Obj, _From, #?STATE{ pasture_event = {StackCount,StackList} , bs = BS, m_ready=R} = State) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,BS, R),
     {reply,ok,State#?STATE{ pasture_event = {NewStackCount,NewStackList}}};

 handle_call(#pasture_group{} = Obj, _From, #?STATE{ pasture_group = {StackCount,StackList} , bs = BS, m_ready=R} = State) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,BS, R),
     {reply,ok,State#?STATE{ pasture_group = {NewStackCount,NewStackList}}};

 handle_call(#pasture_member{} = Obj,_From, #?STATE{ pasture_member = {StackCount,StackList}, bs = BS, m_ready=R}= State) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,BS, R),
     {reply,ok,State#?STATE{ pasture_member = {NewStackCount,NewStackList}}};

 handle_call(#pasture_venue{} = Obj, _From, #?STATE{ pasture_venue = {StackCount,StackList} , bs = BS, m_ready=R} = State) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,BS, R),
     {reply,ok,State#?STATE{ pasture_venue = {NewStackCount,NewStackList}}};

handle_call(#pasture_twitter{} = Obj, _From, #?STATE{ pasture_twitter = {StackCount,StackList} , bs = BS, m_ready=R} = State) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,BS, R),
     {reply,ok,State#?STATE{ pasture_twitter = {NewStackCount,NewStackList}}};

handle_call(#pasture_twitter_location{} = Obj,_From, State) ->
     pasture_twitter_location:inc([Obj#pasture_twitter_location.location]),
     {reply,ok,State}.

handle_cast({new_batch_size, Size}, State) ->
    {noreply, State#?STATE{bs=Size}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {noreply, State#?STATE{m_ready=mnesia_init()}};
handle_info({'EXIT', _From, Reason},#?STATE{pasture_event  = {_,_Se},
                                            pasture_group  = {_,_Sg},
                                            pasture_member = {_,_Sm},
                                            pasture_venue  = {_,_Sv}
                                            } = State) ->
    %% TODO: finish rollback...
    % ?INFO("db batch exit : ~p",[Reason]),
    % ?INFO("Commit stack's before exiting..."),
    % lists:foreach(fun(SL) -> commit(SL) end, [Se,Sg,Sm,Sv]),
    {stop,Reason,State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----

maybe_commit(StackCount,StackList,Obj,BS,true) ->
    case StackCount+1 of
        NewStackCount when NewStackCount >= BS ->
            commit(StackList),
            {0,[]};
        NewStackCount ->
            {NewStackCount,[Obj|StackList]}
    end;
maybe_commit(StackCount,StackList,Obj,_BS,_) ->
    {StackCount+1,[Obj|StackList]}.

commit(StackList) ->
    lists:foreach(fun(Rec) ->
        mnesia:async_dirty(fun() -> mnesia:dirty_write(Rec) end)
    end, StackList).

%% This needs improvement.......
mnesia_init() ->
    mnesia:set_debug_level(verbose),
    ?INFO("Db init...\n"),
    {ok,Master} = application:get_env(pasture, master_db_node),
    ?INFO("Master node : ~p\n",[Master]),
    MnesiaTbls=[pasture_event,
                pasture_group,
                pasture_venue,
                pasture_member,
                pasture_twitter,
                pasture_twitter_location],
    mnesia_init(MnesiaTbls,Master).

mnesia_init(MnesiaTbls,MasterNode) when MasterNode == node() ->
    {ok, Nodes} = application:get_env(pasture, db_nodes),
    [ExtraNodes] = [ Nodes -- [node()] ],
    ?INFO("Extra nodes : ~p\n\n",[ExtraNodes]),
    {ok,_} = mnesia:change_config(extra_db_nodes, ExtraNodes),
    stopped = mnesia:stop(),
    ?INFO("Trying to install schema on ~p\n\n",[Nodes]),
    timer:sleep(25),
    case mnesia:create_schema(Nodes) of
        ok ->
            ?INFO("Schema created ...\n"),
            ok = mnesia:start();
        {error,{NNN,{already_exists,NNN}}} ->
            ?INFO("Schema already created on ~p ...\n",[NNN]),
            ok = mnesia:start()
    end,
    lists:foreach(fun(RN) ->
        ok = rpc:call(RN, mnesia, start, [])
    end, ExtraNodes),
    ?INFO("Mnesia started..."),
    ?CRITICAL("About to create tables : ~p", [MnesiaTbls]),
    lists:foreach(fun(Tbl) ->
        ?CRITICAL("Creating table ~p ...",[Tbl]),
        case Tbl:create_table([MasterNode]) of
            {atomic,ok} ->
                ?CRITICAL("Table ~p created ...", [Tbl]);
            Columns when is_list(Columns) ->
                ?CRITICAL("Table ~p already created ...", [Tbl])
        end,
        lists:foreach(fun(EN) ->
            ?CRITICAL("Add table copy ~p on node ~p ...",[Tbl, EN]),
            EnDiscCopies = rpc:call(EN, mnesia, table_info, [Tbl,disc_only_copies]),
            case lists:member(EN, EnDiscCopies) of
                true ->
                    ?CRITICAL("Already has a copy");
                false ->
                    {atomic,ok} = mnesia:add_table_copy(Tbl, EN, disc_only_copies),
                    ?CRITICAL("Added table copy of ~p to ~p ...", [Tbl, EN])
            end
        end, ExtraNodes)
    end, MnesiaTbls),
    ok = mnesia:wait_for_tables(MnesiaTbls, infinity),
    true;
mnesia_init(_MnesiaTbls,_MasterNode) ->
    stopped = mnesia:stop().