-module (pasture_meetup).

-export([start_link/0
        ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, pasture_meetup_state).

-include("../include/pasture.hrl").

-record(?STATE,{
    ibrowse_req_id, %% {ibrowse_req_id,{1418,674119,867248}}
    stack=[]
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    {ok, #?STATE{}}.

handle_call({ibrowse_req_id,ReqId}, _From, State) ->
    ?INFO("handle_call : ~p\n",[{ibrowse_req_id,ReqId}]),
    {reply, ok, State#?STATE{ibrowse_req_id=ReqId}};
handle_call(Request, _From, State) ->
    ?INFO("handle_call : ~p\n",[Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?INFO("handle_cast : ~p\n",[Msg]),
    {noreply, State}.

handle_info({ibrowse_async_headers,_,"200",_Headers},State) ->
    {noreply,State};
handle_info({ibrowse_async_response,_,Data},
    #?STATE{ stack = Stack } = State) ->
        %%?INFO("PROCESS STACK LENGTH : ~p\n",[length(Stack)]),
        %% TODO: Build something that resolves massive stacks...
        %%       For now, i assume that C:E meant the json was
        %%       incomplete :)
        {ok,NewStack} = parse_json(Stack,Data),
        %%?INFO("NEW STACK LENGTH : ~p\n",[length(NewStack)]),
        {noreply,State#?STATE{ stack = NewStack }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_json(Stack,Data) ->
    FullStack = lists:append(Stack,Data),
    case string:tokens(FullStack,"\n") of
	[Obj] ->
	    {ok,Rest} = parse_json_list([Obj]),
	    {ok,Rest};
        JsonObjs when is_list(JsonObjs), length(JsonObjs) > 1 ->
            {ok,Rest} = parse_json_list(JsonObjs),
            %%io:format("Rest:~p\n",[Rest]),
            {ok,Rest}
    end.

parse_json_list([]) ->
    {ok,[]};
parse_json_list([H|T]) ->
    %%io:format("H : ~p\n",[list_to_binary(H)]),
    try
        Response = jsx:decode(list_to_binary(H)),
        %%io:format("Response : ~p\n",[Response]),
        mnesia:dirty_write(#?MODULE{entry=Response}),
        parse_json_list(T)
    catch
        C:E ->
            %%io:format("C:~p\nE:~p\n~p\n",[C,E,erlang:get_stacktrace()]),
            {ok,lists:flatten(lists:append(H,T))}
    end.