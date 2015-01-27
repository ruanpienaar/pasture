-module (pasture_meetup).

-export([start_link/0
        ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, pasture_meetup_state).

-include("../include/pasture.hrl").

-record(?STATE,{
    ibrowse_req_id,
    stack=[]
}).

%% TODO: the supervisor didn't restart the process when it failed...
% 2015-01-15 23:58:30.856 [error] <0.3914.0> Supervisor pasture_sup had child pasture_meetup_id started with {pasture_meetup,start_link,undefined} at <0.4045.0> exit with reason no match of right hand value {error,unknown_req_id} in pasture_meetup:handle_info/2 line 58 in context child_terminated

start_link() ->
    gen_server:start_link(?MODULE, {}, []).

init({}) ->
    false = process_flag(trap_exit, true),
    {ibrowse_req_id,ReqId} =
        ibrowse:send_req(
            "http://stream.meetup.com/2/rsvps",[],get,[],
            [ {stream_chunk_size,1024 * 2},
              {stream_to,{self(), once}}
            ],
            infinity),
    {ok,#?STATE{ibrowse_req_id=ReqId}}.

handle_call(undefined, _From, State) ->
    {reply, ok, State}.

handle_cast(undefined, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason},#?STATE{ibrowse_req_id = RI} = State) ->
    ?INFO("pasture_meetup exit ~p\n",[Reason]),
    ok = ibrowse:stream_close(RI),
    {noreply,State#?STATE{ibrowse_req_id=undefined}};
handle_info({ibrowse_async_headers,ReqId,"200",Headers},State) ->
    ?INFO("ibrowse_async_headers : ~p\n",[Headers]),
    case ibrowse:stream_next(ReqId) of
        {error,unknown_req_id} ->
            {stop, normal, State};
        ok ->
            {noreply,State}
    end;

handle_info({_,
                _,{error,connection_closed}},#?STATE{ stack = _ } = State) ->
	exit(whereis(?MODULE),restart);
handle_info({ibrowse_async_response,
                NewReqId,Data},#?STATE{ stack = Stack } = State) ->
        %%?INFO("Next\n",[]),
        %%?INFO("PROCESS STACK LENGTH : ~p\n",[length(Stack)]),
        %% TODO: Build something that resolves massive stacks...For now, i assume that C:E meant the json was
        %%       incomplete :)

        %% TODO: how do i exit/or handle the exit better, rather
        %% than a bad match.
        case parse_json(Stack,Data) of
            {ok,NewStack} ->
                case ibrowse:stream_next(NewReqId) of
                    {error,unknown_req_id} ->
                        exit(whereis(?MODULE),restart);
                    ok ->
                        {noreply,
                            State#?STATE{ stack = NewStack,
                                          ibrowse_req_id =NewReqId }}
                end;
            error ->
                exit(whereis(?MODULE),restart);
	   Else ->
		exit(whereis(?MODULE),restart)
        end.

terminate(Reason, #?STATE{ibrowse_req_id = RI} = _State) ->
    ?INFO("pasture_meetup terminate ~p\n",[Reason]),
    ibrowse:stream_close(RI),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_json(Stack,Data) ->
    try
        FullStack = lists:append(Stack,Data),
        case string:tokens(FullStack,"\n") of
        	[Obj] ->
        	    parse_json_list([Obj]);
            JsonObjs when is_list(JsonObjs) ->
                parse_json_list(JsonObjs)
        end
    catch
        C:E ->
            ?INFO("parse_json\nC:~p\nE:~p\n~p\n",
                        [C,E,erlang:get_stacktrace()]),
            error
    end.

parse_json_list([]) ->
    {ok,[]};
parse_json_list([H|T]) ->
    BinH = list_to_binary(H),
    try
        Json = jsx:decode(BinH),
        ok = pasture_db:json_to_recs(Json),
        parse_json_list(T)
    catch
        error:Reason when Reason =:= badarg;
                          Reason =:= {case_clause,initialdecimal};
                          Reason =:= {case_clause,negative} ->
            {ok,lists:flatten(lists:append(H,T))};
        error:{case_clause,MissingClause} ->
            ?INFO("pasture_meetup error:{case_clause,~p}\n",[MissingClause]),
            ?INFO("Tried decoding ~p\n",[BinH]),
            error;
        C:E ->
            ?INFO("parse_json_list\nC:~p\nE:~p\n~p\n",
                        [C,E,erlang:get_stacktrace()]),
            error
    end.
