-module (nodes_watchdog).
-export([start_link/0,
         subscribe_and_pause/0
        ]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/nodes.hrl").

-define(SERVER, ?MODULE).
-define(STATE, nodes_watchdog).
-record(?STATE,{ ref }).

%% ---

start_link() ->
    ?INFO("start_link()...!!! "),
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

subscribe_and_pause() ->
    gen_server:call(?SERVER,subscribe_and_pause,infinity).

%% ---

init({}) ->
    {ok,Nodes} = application:get_env(nodes, db_nodes),
    ok = connect_nodes(Nodes),
    ok = net_kernel:monitor_nodes(true),
    {ok, #?STATE{}}.

handle_call(subscribe_and_pause,From,State) ->
    ?INFO("Paused and subscribing to mnesia system...."),
    N = node(),
    {ok,N} = mnesia:subscribe(system),
    %% stopped = mnesia:stop(),
    {noreply, State#?STATE{ ref = From } };
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% TODO: handle the nodes going down............

% handle_info({nodeup, Node}, State) ->
%     {noreply, State};
% handle_info({nodedown, Node}, State) ->
%     {noreply, State};

handle_info({mnesia_system_event,{mnesia_up,_Node}},
             #?STATE{ ref = From } = State) ->
    %% ok = mnesia:start(),
    {_,ok} = gen_server:reply(From,ok),
    {noreply, State#?STATE{ ref = undefined }};
handle_info(Event, State) ->
    ?INFO("GOT EVENT ~p\n",[Event]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect_nodes(Nodes) ->
    connect_nodes(Nodes,undefined).

connect_nodes(_Nodes,_S=done) ->
    ok = net_kernel:monitor_nodes(true);
connect_nodes(Nodes,S=undefined) ->
    F = fun(N) ->
            net_kernel:connect(N)
        end,
    timer:sleep(250),
    case lists:all(F, Nodes) of
        true  -> connect_nodes(Nodes,done);
        false -> connect_nodes(Nodes,S)
    end.