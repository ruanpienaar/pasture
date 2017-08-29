-module(pasture_meetup_sup).
-behaviour(supervisor).

-export([
    start_link/0
]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    case application:get_env(pasture, master_db_node)=={ok,node()} of
        true ->
            do_init();
        _ ->
            no_init()
    end.

no_init() ->
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, []}}.

do_init() ->
    C1 =
        {pasture_meetup,
            {pasture_meetup, start_link, []},
            permanent, 5000, worker,
            [pasture_meetup]},
    Children = [C1],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.