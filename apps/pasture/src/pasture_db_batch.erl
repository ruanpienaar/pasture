-module (pasture_db_batch).

-export([start_link/0,
         add/1
        ]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/pasture.hrl").

-define(SERVER, ?MODULE).

-define(STATE, pasture_db_batch_state).
-record(?STATE,{ bs,
                 pasture_event  = {0,[]},
                 pasture_group  = {0,[]},
                 pasture_member = {0,[]},
                 pasture_venue  = {0,[]}
}).

% ---

add({Type,Obj}) ->
    gen_server:call(?MODULE,{Type,Obj},infinity).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

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
     {reply,ok,State#?STATE{ pasture_venue = {NewStackCount,NewStackList}}}.

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
            case StackCount+1 of
                NewStackCount when NewStackCount >= BS ->
                    ?INFO("Trying to commit ~p...\n",[BS]),
                    commit(StackList),
                    ?INFO("commited ~p successfully...\n",[BS]),
                    {0,[]};
                NewStackCount ->
                    {NewStackCount,[Obj|StackList]}
            end
        end.

commit(StackList) ->
    lists:foreach(
        fun(Rec) ->
            ok = mnesia:async_dirty(fun() ->
                mnesia:dirty_write(Rec)
            end)
        end, StackList).