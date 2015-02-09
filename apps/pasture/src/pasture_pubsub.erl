-module (pasture_pubsub).

-behaviour(gen_server).

-include("../include/pasture.hrl").

-export ([subscribe/1,
          notify/2]).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

subscribe(EventType) ->
    %% Gproc notation: {p, l, Name} means {(p)roperty, (l)ocal, Name}
    gproc:reg({p, l, {?MODULE, EventType}}).

notify(EventType, Msg) ->
    Key = {?MODULE, EventType},
    gproc:send({p, l, Key}, {self(), Key, Msg}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    true = pasture_pubsub:subscribe(apples),
    true = gproc:reg({n, l, notification_count}, 0),
    {ok, undefined}.

handle_call(Request, _From, State) ->
    ?INFO("handle_call ~p",[Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    ?INFO("handle_cast ~p",[Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO("handle_info ~p",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?INFO("terminate ~p",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.