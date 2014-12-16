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
    stack=[],
    entry_started=false
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

handle_info({ibrowse_async_response,_ReqId,Data},
    #?STATE{ stack = Stack } = State)
        %% when State#?STATE.ibrowse_req_id == ReqId
        ->
    %% ?INFO("Data : \n~p\n\n",[Data]),
    ?INFO("Received data...\n"),
    {noreply,State#?STATE{ stack = parse_json(Stack,Data) }};
handle_info(Info, State) ->
    ?INFO("handle_info : ~p\n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_json([],Data) ->
    Data;
parse_json(Stack,Data) ->
    FullStack = lists:append(Stack,Data),
    try
        case re:run(FullStack, "([a-zA-Z0-9]+)\\r\\n(.*)", [global]) of
            {match,[MS=[{_,End},{_,_},{Start,Length}]|_]} ->
                ?INFO("     ...MS : ~p.\n",[MS]),
                ?INFO("     ...match...\n"),
                %% Could also just use the hex in the string
                % +1 i think because of ".
                FullEntry = string:substr(FullStack,Start+1,Length),
                %% io:format("FullEntry : ~p\n\n",[FullEntry]),
                mnesia:dirty_write(#?MODULE{entry=FullEntry}),
                % +3 to remove \n\r\n
                %%io:format("FullStack : ~p\n\n",[FullStack]),
                % io:format("Rest = lists:nthtail(~p+5,~p)",
                %     [End,FullStack]),
                case length(FullStack) >= End+5 of
                    true ->
                        Rest = lists:nthtail(End+5,FullStack),
                        parse_json(Rest,[]);
                    false ->
                        FullStack
                end;
            {match,MM} ->
                ?INFO("     ...match with ~p\nData: ~p\n\n",[MM,Data]),
                FullStack;
            nomatch ->
                ?INFO("     ...no match\n",[]),
                FullStack
        end
    catch
        error:badarg ->
            ?WARNING("parse_json Crash\n~p\n",[erlang:get_stacktrace()]),
            %% Clear the whole stack
            [];
        C:E ->
            %% Clear the whole stack
            ?WARNING("parse_json failure\n~p\n~p\n~p\n",
                [C,E,erlang:get_stacktrace()]),
            []
    end.
