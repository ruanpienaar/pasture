-module (pasture_db_batch).

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
                 pasture_twitter = {0,[]}
}).

% ---

%% TODO: make the db batch, increase in size, when a node is down,
%% and when mnesia aborts, because the majotiry flag is set to true.

add({Type,Obj}) ->
    io:format("M", []),
    gen_server:call(?MODULE,{Type,Obj},infinity).

new_batch_size(Size) when is_integer(Size) andalso Size > 0 ->
    application:set_env(pasture, batch_size, Size),
    gen_server:cast(?MODULE, {new_batch_size, Size}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

%% ----

init({}) ->
    false = process_flag(trap_exit, true),
    {ok,BS} = application:get_env(pasture, batch_size),
    {ok, #?STATE{bs=BS}}.

 handle_call({pasture_event,Obj},_From,
            #?STATE{ pasture_event = {StackCount,StackList} , bs = BS} = State
        ) ->
     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList,Obj,                                              BS),
     {reply,ok,State#?STATE{ pasture_event = {NewStackCount,NewStackList}}};

 handle_call({pasture_group,Obj},_From,
            #?STATE{ pasture_group = {StackCount,StackList} , bs = BS} = State
        ) ->
     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList,Obj,                                              BS),
     {reply,ok,State#?STATE{ pasture_group = {NewStackCount,NewStackList}}};

 handle_call({pasture_member,Obj},_From,
            #?STATE{ pasture_member = {StackCount,StackList}, bs = BS }= State
        ) ->
     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList,Obj,                                              BS),
     {reply,ok,State#?STATE{ pasture_member = {NewStackCount,NewStackList}}};

 handle_call({pasture_venue,Obj},_From,
            #?STATE{ pasture_venue = {StackCount,StackList} , bs = BS} = State
        ) ->
     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList,Obj,                                              BS),
     {reply,ok,State#?STATE{ pasture_venue = {NewStackCount,NewStackList}}};

handle_call({pasture_twitter,Obj},_From,
            #?STATE{ pasture_twitter = {StackCount,StackList} , bs = BS} = State
        ) ->
     {NewStackCount,NewStackList} = maybe_commit(StackCount,StackList,Obj,                                              BS),
     {reply,ok,State#?STATE{ pasture_twitter = {NewStackCount,NewStackList}}};

handle_call({pasture_twitter_location,Obj},_From, State) ->
     pasture_twitter_location:inc([Obj#pasture_twitter_location.location]),
     {reply,ok,State}.

handle_cast({new_batch_size, Size}, State) ->
    {noreply, State#?STATE{bs=Size}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason},#?STATE{pasture_event  = {_,Se},
                                            pasture_group  = {_,Sg},
                                            pasture_member = {_,Sm},
                                            pasture_venue  = {_,Sv}
                                            } = State) ->
    ?INFO("db batch exit : ~p",[Reason]),
    ?INFO("Commit stack's before exiting..."),
    lists:foreach(fun(SL) -> commit(SL) end, [Se,Sg,Sm,Sv]),
    {stop,normal,State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----

append_or_commit(StackCount,StackList,Obj,BS) ->
    case lists:member(Obj, StackList) of
        true ->
            {StackCount,StackList};
        false ->
            maybe_commit(StackCount,StackList,Obj,BS)
        end.

maybe_commit(StackCount,StackList,Obj,BS) ->
    case StackCount+1 of
        NewStackCount when NewStackCount >= BS ->
            %% ?INFO("Trying to commit ~p...\n",[BS]),
            commit(StackList),
            %%?INFO("commited ~p successfully...\n",[BS]),
            {0,[]};
        NewStackCount ->
            {NewStackCount,[Obj|StackList]}
    end.

commit(StackList) ->
    lists:foreach(fun(Rec) ->
        mnesia:async_dirty(fun() -> mnesia:dirty_write(Rec) end)
    end, StackList).