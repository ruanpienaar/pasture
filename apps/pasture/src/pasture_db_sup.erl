-module (pasture_db_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @private
init({}) ->
    Child1 =
        {pasture_db_batch,
            {pasture_db_batch, start_link, []},
            permanent, 5000, worker,
            [pasture_db_batch]},

    Children = [Child1],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.