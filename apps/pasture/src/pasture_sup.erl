-module(pasture_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/0,
    start_children/1
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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    C = pasture_meetup,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
          [{pasture_meetup_id,{C, start_link, []}, permanent, 100, worker, [C]}
          ]}
    }.

start_child() ->
    supervisor:start_child(?MODULE, []).

start_children(X) ->
    [ start_child() || _N <- lists:seq(1,X) ].