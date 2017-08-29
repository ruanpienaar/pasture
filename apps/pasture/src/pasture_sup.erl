-module(pasture_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

init({}) ->
    C1 =
        {pasture_meetup_sup,
            {pasture_meetup_sup, start_link, []},
            permanent, 5000, worker,
            [pasture_meetup_sup]},
    C2 =
        {pasture_twitter_sup,
            {pasture_twitter_sup, start_link, []},
            permanent, 5000, worker,
            [pasture_twitter_sup]},
    C3 =
        {pasture_google_trends_sup,
            {pasture_google_trends_sup, start_link, []},
            permanent, 5000, worker,
            [pasture_google_trends_sup]},
    C4 =
        {pasture_db_sup,
            {pasture_db_sup, start_link, []},
            permanent, 5000, worker,
            [pasture_db_sup]},

    Children = [
	C1,
        C2,C3,C4],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.