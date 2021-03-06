-module(pasture_web).
-export([start/0,
         stop/0,
         twitter/0
    ]).
    
-define(COWBOY_REF, http).

start() ->
    {ok,Port} = port(),
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_http(?COWBOY_REF,
                                _ConnectionPoolSize=10,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {max_keepalive, 50},
                                 %% {onrequest, fun timely_session:on_request/1},
                                 {timeout, 500}
                                ]
                               ),
    {ok,Pid}.

    routes() ->
        [
         {'_', [ {"/pasture_member", pasture_member, []},
                 {"/pasture_member/:member_id", pasture_member, []},

                 {"/pasture_event", pasture_event, []},
                 {"/pasture_event/:event_id", pasture_event, []},

                 {"/pasture_group", pasture_group, []},
                 {"/pasture_group/:group_id", pasture_group, []},

                 {"/pasture_venue", pasture_venue, []},
                 {"/pasture_venue/:venue_id", pasture_venue, []},

                 {"/", cowboy_static, {priv_file, pasture, "www/index.html"}},
                 {"/[...]", cowboy_static, {priv_dir, pasture, "/www"}}
               ]}
        ].

    port() ->
        case os:getenv("PORT") of
            false -> application:get_env(pasture,http_port);
            Other -> {ok,list_to_integer(Other)}
        end.

stop() ->
    cowboy:stop_listener(?COWBOY_REF).

twitter() ->
    _URL = "https://stream.twitter.com/1.1/statuses/filter.json".