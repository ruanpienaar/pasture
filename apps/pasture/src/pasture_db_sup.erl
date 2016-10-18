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
    Child2 =
        {pasture_db_esqlite,
            {pasture_db_esqlite, start_link, []},
            permanent, 5000, worker,
            [pasture_db_esqlite]},

    Children = [Child1, Child2],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.