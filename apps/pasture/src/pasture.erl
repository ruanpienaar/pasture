-module(pasture).
-export([start/0]).

start() ->
    [ ok=application:start(X) || X <-
        [sasl, ibrowse, inets, erlsom, syntax_tools, compiler, goldrush, lager,
         crypto, asn1, public_key, ssl, mnesia, ranch, cowlib, cowboy, erlport, pasture]
    ].