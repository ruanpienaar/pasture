-module (pasture_db_batch).

-export([start_link/0,
         add/1
        ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(STATE, pasture_db_batch_state).
-record(?STATE,{ pasture_event  = {0,[]},
                 pasture_group  = {0,[]},
                 pasture_member = {0,[]},
                 pasture_venue  = {0,[]}
}).

% ---

add({Type,Obj}) ->
    gen_server:call(?MODULE,{Type,Obj}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%% ----

init({}) ->
    {ok, #?STATE{}}.

handle_call({pasture_event,Obj},_From,
        #?STATE{ pasture_event = {StackCount,StackList} } = State) ->
    {NewStackCount,NewStackList} =
        append_or_commit(StackCount,StackList,Obj),
    {reply,ok,#?STATE{ pasture_event = {NewStackCount,NewStackList}}}.

% handle_call({pasture_event,Obj},_From,
%         #?STATE{ {StackCount,StackList} = pasture_event } = State) ->
%     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList),
%     {reply,ok,#?STATE{ pasture_event = {NewStackCount,NewStackList}}};
% handle_call({pasture_group,Obj},_From,
%         #?STATE{ {StackCount,StackList} = pasture_group } = State) ->
%     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList),
%     {reply,ok,#?STATE{ pasture_group = {NewStackCount,NewStackList}}};
% handle_call({pasture_member,Obj},_From,
%         #?STATE{ {StackCount,StackList} = pasture_member } = State) ->
%     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList),
%     {reply,ok,#?STATE{ pasture_member = {NewStackCount,NewStackList}}};
% handle_call({pasture_venue,Obj},_From,
%         #?STATE{ {StackCount,StackList} = pasture_venue } = State) ->
%     {NewStackCount,NewStackList} = append_or_commit(StackCount,StackList),
%     {reply,ok,#?STATE{ pasture_venue = {NewStackCount,NewStackList}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----

append_or_commit(StackCount,StackList,Obj) ->
    case StackCount+1 of
        NewStackCount when NewStackCount >= 500 ->
            {atomic,ok} =
                mnesia:transaction( fun() ->
                    lists:foreach(fun(Rec) -> mnesia:write(Rec) end, StackList)
                end),
            {0,[]};
        NewStackCount ->
            {NewStackCount,[Obj|StackList]}
    end.