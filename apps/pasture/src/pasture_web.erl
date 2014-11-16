-module(pasture_web).
-export([start/0]).

start() ->
    {ok,Port} = port(),
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_http(http,
                                ConnectionPoolSize=10,
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
         {'_', [
                    {"/[...]",  cowboy_static, {priv_dir, pasture, "/www"}}
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
    % "oauth_access_token", "101055484-NVfKL3Tr1LioLunboyIvqPW8z4sE8hgVUlJkVRYB",
    % "oauth_access_token_secret", "EBoeXKuK3oLWEExK6otVGEzc7hXgF50ZJhBZBNICbifQV",
    % "consumer_key", "VxVSRMkD97tJSm3iBg0fbatBR",
    % "consumer_secret", "UGfTwIh1DRM0xKQYGtYQjV8maSmiTJSfghmT1KKunU1K5rK03l"


    URL = "https://stream.twitter.com/1.1/statuses/filter.json".