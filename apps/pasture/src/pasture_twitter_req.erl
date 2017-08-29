-module (pasture_twitter_req).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    self() ! start,
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State) ->
    ibrowse:send_req(
        "https://stream.twitter.com/1.1/statuses/sample.json",
        [{
            "Authorization",
            "OAuth oauth_consumer_key=\"ZPeKV4XvbjY8DiOnQJSQPeQVn\", oauth_nonce=\"c35640e3a8914db48e037c9bdd2537fc\", oauth_signature=\"0eHcQPsCN5jTw4oWqwwlhyHPNmI%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1466887904\", oauth_token=\"101055484-iQg2OaLNtEHInKDPX2Ca6MrCBkBZ74G94iHAPgJm\", oauth_version=\"1.0\""
        }],
        get,
        [],
        [{stream_to, self()}]
    ),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Info : ~p\n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





    % curl --get 'https://stream.twitter.com/1.1/statuses/sample.json' --header 'Authorization: OAuth oauth_consumer_key="hJY9CmIS9ja3CsOvKyQov9OB5", oauth_nonce="b65096ab317e35ee93c4244abd3c82d6", oauth_signature="%2BXVTr5AiJQ1F3K0qZT9MFnqVxJ8%3D", oauth_signature_method="HMAC-SHA1", oauth_timestamp="1466883341", oauth_token="101055484-PfPLXpERGmZhSKzQJYg1yzamdqJ7fpF6y6EkwhXi", oauth_version="1.0"' --verbose