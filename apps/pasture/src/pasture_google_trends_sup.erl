-module(pasture_google_trends_sup).
-export([start_link/0]).

-behaviour(supervisor).

%% API
-export([start_link/0,
         countries/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Type, Args),
    {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

-define(CHILD(Id, Mod, Func, Type, Args),
    {Id, {Mod, Func, Args}, permanent, 5000, Type, [Mod]}).

countries() ->
    [
        {1, usa},
        {3, india},
        {4, japan},
        {5, singapore},
        {6, israel},
        {8, australia},
        {9, uk},
        {10, hong_kong},
        {12, taiwan},
        {13, canada},
        {14, russia},
        {15, germany},
        {16, france},
        {17, netherlands},
        {18, brazil},
        {19, indonesia},
        {21, mexico},
        {23, south_korea},
        {24, turkey},
        {25, philippines},
        {26, spain},
        {27, italy},
        {28, vietnam},
        {29, egypt},
        {30, argentina},
        {31, poland},
        {32, columbia},
        {33, thailand},
        {34, malaysia},
        {35, ukraine},
        {36, saudi_arabia},
        {37, kenya},
        {38, chile},
        {39, romania},
        {40, south_africa},
        {41, belgium},
        {42, sweden},
        {43, czech},
        {44, austria},
        {45, hungary},
        {46, switzerland},
        {47, portugal},
        {48, greece},
        {49, denmark},
        {50, finland},
        {51, norway},
        {52, nigeria}
    ].

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 9600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
        [ ?CHILD(Country, pasture_gtw, worker, [{Id,Country}])
        || {Id,Country} <- countries() ]
        }
    }.