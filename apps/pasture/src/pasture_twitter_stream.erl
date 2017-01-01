-module (pasture_twitter_stream).

-include("../include/pasture.hrl").

-export([start_link/1,
         stop/1,
         start/1,
         python_cast/1]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PYTHON_MOD, tweepy_twitter_stream).
-define(STATE,pasture_twitter_stream_state).
-record(?STATE, {
    python_pid,
    filter_str,
    search_result_count = 0
}).

start_link(Str) ->
    gen_server:start_link(?MODULE, [Str], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

start(Str) ->
    gen_server:call(?MODULE,{start,Str}).

python_cast(Msg) ->
    gen_server:call(?MODULE, {python_cast, Msg}).

init([Str]) ->
    process_flag(trap_exit, true),
    {ok, P} = python:start([{python_path, code:priv_dir(pasture)},
                            {python, "python2"}
                            ]),

    %% TODO: send OAUTH keys, to PYTHON!!!!!!!

    ok = python:call(P,?PYTHON_MOD,register_handler,[self()]),
    ok = python:cast(P,{start,Str}),
    {ok, #?STATE{python_pid=P,
                 filter_str=Str}}.

handle_call({python_cast,Msg}, _From, #?STATE{ python_pid = P} = State) ->
    R = python:cast(P, Msg),
    io:format("python:cast ~p Response : ~p\n",[Msg,R]),
    {reply,ok,State};
handle_call(stop, _From, State) ->
    ?INFO("Stop : gs call stop",[]),
    {stop, normal, State};
handle_call(Request, _From, State) ->
    io:format("handle call ~p\n",[Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Handle_cast ~p \n",[Msg]),
    {noreply, State}.

handle_info(stop, State) ->
    ?INFO("Stop : ! stop\n",[]),
    {stop, stop, State};

handle_info({'EXIT', From, Reason}, State) when is_port(From) ->
    ?INFO("PORT !!! Stop : FROM : ~p EXIT ~p",[From, Reason]),
    {noreply, State};
handle_info({'EXIT', From, Reason}, State) ->
    ?INFO("Stop : FROM : ~p EXIT ~p",[From, Reason]),
    {stop, stop, State};

handle_info([error,ResponseCode], #?STATE{ python_pid = P} = State) ->

    %% Write count to pasture_twitter_search.........

    ?INFO("Stopped because tweepy responded with ~p",[ResponseCode]),
    ok = python:stop(P),
    {stop, ResponseCode, State};
handle_info(Info, #?STATE{filter_str=Str} = State) when is_list(Info) ->
    %%io:format("\n~p\n",[Info]),
    case jsx:decode(list_to_binary(Info)) of
        [{<<"limit">>,[{<<"track">>,_},{<<"timestamp_ms">>,_}]}] ->
            ok;
        DecJson ->
            case lists:keyfind(<<"id">>, 1, DecJson) of
    	        false ->
    	            io:format("dropped\n~p\n", [DecJson]);
	        {<<"id">>,Id} ->
                    ok = pasture_db_esqlite:add(#pasture_twitter{id = Id, filter_str=Str,json = Info})
            end
    end,
    {noreply, State#?STATE{ search_result_count = State#?STATE.search_result_count + 1 }};
handle_info(Info, State) ->
    io:format("Handle_info ~p \n",[Info]),
    {noreply, State}.

terminate(Reason, #?STATE{ python_pid = P} = _State) ->
    %% Write count to pasture_twitter_search.........
    ok = python:stop(P),
    io:format("Terminate ~p \n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
