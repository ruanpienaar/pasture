-module (pasture_twitter_fsm).
-export([
    start_link/0,
    stop/0
]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, terminate/3, code_change/4, handle_event/3, handle_sync_event/4]).


-export([
    %% Initial state
    off/2, off/3,
    %% Second step
    on/2, on/3,
    retry/2, retry/3
]).

-define(SERVER, ?MODULE).
-define(STATE,pasture_twitter_fsm_state).
-record(?STATE,{}).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, {}, []).

stop() ->
    make_stopped_state.

init({}) ->
    {ok, off, #?STATE{}}.


off(on, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"off/2"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"on"]),
    {next_state, on, State}.

off(on, _From, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"off/3"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"on"]),
    {reply, ok, on, State}.


on(off, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"on/2"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"off"]),
    {next_state, off, State}.

on(off, _From, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"on/3"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"off"]),
    {reply, ok, off, State}.


retry(some_action, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"retry/2"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"connecting"]),
    {next_state, connecting, State}.

retry(some_action, _From, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,"retry/3"]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,"connecting"]),
    {reply, ok, connecting, State}.


handle_event(_Event, StateName, State) ->
    io:format(" ... [~p] ... state was        : ~p \n",[?MODULE,"handle_event/3"]),
    io:format(" /// [~p] /// StateName is : ~p \n",[?MODULE,StateName]),
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,state]),
    io:format(" /// [~p] /// StateName is : ~p \n",[?MODULE,StateName]),
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,state]),
    io:format(" /// [~p] /// StateName is : ~p \n",[?MODULE,StateName]),
    {next_state, StateName, State}.

terminate(Reason, StateName, _State) ->
    io:format(" ... terminate ... Reason : ~p \n",[Reason]),
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,state]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,StateName]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    io:format(" ... Code Change ...\n",[]),
    io:format(" ... [~p] ... state was     : ~p \n",[?MODULE,state]),
    io:format(" /// [~p] /// next state is : ~p \n",[?MODULE,next_state]),
    {ok, StateName, State}.

% connect() ->
%     io:format("\n\n ... CONNECTING ... \n\n").

% stream() ->
%     io:format("\n\n ... STREAMING ... \n\n",[]).