-module (pasture_twitter_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/1,
    children/0
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
    C = pasture_twitter_stream,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
          [{pasture_twitter_stream_id,{C, start_link, []},temporary, 1000, worker, [C]}
          ]}
    }.

start_child(Str) ->
    supervisor:start_child(?MODULE, [Str]).

children() ->
    supervisor:which_children(?MODULE).