-module(pasture_web).
-export([start/0,
         stop/1,
         twitter/0
    ]).

start() ->
    {ok,Port} = port(),
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_http(http,
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
         {'_', [ {"/pasture_event", pasture_event, []},
                 {"/pasture_event/:event_id", pasture_event, []},
                 {"/", cowboy_static, {priv_file, pasture, "www/index.html"}},
                 {"/[...]", cowboy_static, {priv_dir, pasture, "/www"}}
               ]}
        ].

    port() ->
        case os:getenv("PORT") of
            false -> application:get_env(pasture,http_port);
            Other -> {ok,list_to_integer(Other)}
        end.

stop(Pid) ->
    cowboy:stop_listener(Pid).

twitter() ->
    _URL = "https://stream.twitter.com/1.1/statuses/filter.json".