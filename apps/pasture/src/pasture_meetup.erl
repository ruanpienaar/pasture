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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    false = process_flag(trap_exit, true),
    {ibrowse_req_id,ReqId} =
        ibrowse:send_req(
            "http://stream.meetup.com/2/rsvps",[],get,[],
            [ {stream_chunk_size,1024 * 2},
              %% {stream_to,pasture_meetup}
              {stream_to,{pasture_meetup, once}}
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
    ok = ibrowse:stream_next(ReqId),
    {noreply,State};
handle_info({ibrowse_async_response,
                NewReqId,Data},#?STATE{ stack = Stack } = State) ->
        %%?INFO("Next\n",[]),
        %%?INFO("PROCESS STACK LENGTH : ~p\n",[length(Stack)]),
        %% TODO: Build something that resolves massive stacks...For now, i assume that C:E meant the json was
        %%       incomplete :)
        {ok,NewStack} = parse_json(Stack,Data),
        ok = ibrowse:stream_next(NewReqId),
        {noreply,State#?STATE{ stack = NewStack, ibrowse_req_id = NewReqId }}.

terminate(Reason, #?STATE{ibrowse_req_id = RI} = _State) ->
    ?INFO("pasture_meetup terminate ~p\n",[Reason]),
    ibrowse:stream_close(RI).

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
            lager:info("parse_json\nC:~p\nE:~p\n~p\n",
                        [C,E,erlang:get_stacktrace()]),
            {ok,[]}
    end.

parse_json_list([]) ->
    {ok,[]};
parse_json_list([H|T]) ->
    try
        Json = jsx:decode(list_to_binary(H)),
        ok = pasture_db:json_to_recs(Json),
        parse_json_list(T)
    catch
        error:badarg ->
            {ok,lists:flatten(lists:append(H,T))};
        C:E ->
            lager:info("parse_json_list\nC:~p\nE:~p\n~p\n",
                        [C,E,erlang:get_stacktrace()]),
            exit(whereis(?MODULE),restart)
    end.