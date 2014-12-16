-module(pasture_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_twitter/0
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 500, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_twitter() ->
    gen_server:call(pasture_twitter,update_status).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, _Restarts=5, _WithInSeconds=10}, [
        %% ?CHILD(pasture_twitter_fsm, worker),
        %% ?CHILD(pasture_twitter, worker),
        ?CHILD(pasture_meetup, worker)
    ]}}.

