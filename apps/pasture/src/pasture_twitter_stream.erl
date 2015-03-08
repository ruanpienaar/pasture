-module (pasture_twitter_stream).

-include("../include/pasture.hrl").

-export([start_link/1,
         start/1,
         python_cast/1]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PYTHON_MOD, tweepy_twitter_stream).
-define(STATE,pasture_twitter_stream_state).
-record(?STATE, {python_pid, filter_str}).

start_link(Str) ->
    gen_server:start_link(?MODULE, [Str], []).

start(Str) ->
    gen_server:call(?MODULE,{start,Str}).

python_cast(Msg) ->
    gen_server:call(?MODULE, {python_cast, Msg}).

init([Str]) ->
    {ok, P} = python:start([{python_path, "/Users/rp/code/pasture"},
                            {python, "python2"}]),
    ok = python:call(P,?PYTHON_MOD,register_handler,[self()]),
    ok = python:cast(P,{start,Str}),
    {ok, #?STATE{python_pid=P,
                 filter_str=Str}}.

handle_call({python_cast,Msg}, _From, #?STATE{ python_pid = P} = State) ->
    R = python:cast(P, Msg),
    io:format("python:cast ~p Response : ~p\n",[Msg,R]),
    {reply,ok,State};
handle_call(Request, _From, State) ->
    io:format("handle call ~p\n",[Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Handle_cast ~p \n",[Msg]),
    {noreply, State}.

handle_info(stop, #?STATE{ python_pid = P} = State) ->
    ?INFO("Stop",[]),
    ok = python:stop(P),
    {stop, stop, State};
handle_info([error,ResponseCode], #?STATE{ python_pid = P} = State) ->
    ?INFO("Stopped because tweepy responded with ~p",[ResponseCode]),
    ok = python:stop(P),
    {stop, ResponseCode, State};
handle_info(Info, State) when is_list(Info) ->
    Json = jsx:decode(list_to_binary(Info)),
    Id = proplists:get_value(<<"id">>, Json),
    Obj = pasture_twitter:new([Id,State#?STATE.filter_str,Json]),
    ok = pasture_db_batch:commit([Obj]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Handle_info ~p \n",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminate ~p \n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
