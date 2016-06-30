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

handle_info({'EXIT', _From, Reason},State) ->
    async_restart(Reason,State);
handle_info({ibrowse_async_headers,ReqId,"200",Headers},State) ->
    ?INFO("ibrowse_async_headers : ~p\n",[Headers]),
    case ibrowse:stream_next(ReqId) of
        {error,unknown_req_id} ->
            {stop, normal, State};
        ok ->
            {noreply,State}
    end;
handle_info({ibrowse_async_response,
                _,{error,connection_closed}}, State) ->
    async_restart({error,connection_closed}, State);
handle_info({ibrowse_async_response,
                NewReqId,Data},#?STATE{ stack = Stack } = State) ->
        case parse_json(Stack,Data) of
            {ok,NewStack} ->
                case ibrowse:stream_next(NewReqId) of
                    {error, unknown_req_id} ->
                        async_restart({error,unknown_req_id},State);
                    ok ->
                        {noreply,
                            State#?STATE{ stack = NewStack,
                                          ibrowse_req_id =NewReqId }}
                end;
            error ->
                async_restart({error,parse_json_error},State);
            Else ->
                async_restart({error,Else},State)
        end;
handle_info(Msg, State) ->
    io:format("~p\n", [Msg]),
    {noreply, State}.

async_restart(Reason,State) ->
    ?INFO("Going to restart. Reason:~p \n\n\n",[Reason]),
    exit(self(),restart),
    {noreply,State}.

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
