-module (pasture_twitter_sup).
% -export([start_link/0]).

% -behaviour(supervisor).
% -export([init/1]).

% -define(SERVER, ?MODULE).

% start_link() ->
%     supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

% %% @private
% init({}) ->
%     Child1 =
%         {pasture_twitter,
%             {pasture_twitter, start_link, []},
%             permanent, 5000, worker,
%             [pasture_twitter]},

%     Children = [Child1],
%     RestartStrategy = {one_for_one, 5, 10},
%     {ok, {RestartStrategy, Children}}.