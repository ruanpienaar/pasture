-module(oauth2c_twitter_example).

-export([
    run/0
]).

% -record(client, {
%         grant_type   ,
%         auth_url     ,
%         access_token ,
%         token_type   ,
%         refresh_token,
%         id           ,
%         secret       ,
%         scope
% }).

% %% PUT YOUR OWN VALUES HERE! and DO NOT COMMIT !!! LOL
% -define(CONSUMER_SECRET, <<"">>).
% -define(CONSUMER_KEY, <<"">>).

% -define(OAUTH2_TOKEN_URL, <<"https://api.twitter.com/oauth2/token">>).

% -define(USER_TIMELINE_URL(User, StrCount),
%         <<"https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
%           , User, "&count=", StrCount>>).

% -define(APP_LIMITS_URL(Resources),
%         << "https://api.twitter.com/1.1/application/rate_limit_status.json?resources="
%            , Resources>>).
% run() ->
%     % application:ensure_all_started(oauth2c),
%     % application:ensure_all_started(ssl),

%     % {ok, Headers, Client} =
%     %     oauth2c:retrieve_access_token(
%     %       <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
%     %       ?CONSUMER_SECRET),
%     % io:format("~p ~p ~n", [Headers, Client]),

%     % {{ok, _Status1, _Headers1, Tweets}, Client2} =
%     %     oauth2c:request(
%     %       get, json, ?USER_TIMELINE_URL("raaneipnaur", "4"), [200], Client),
%     % io:format("Tweets: ~p~n", [Tweets]),

%     % {{ok, _Status2, _Headers2, Limits}, _Client3} =
%     %     oauth2c:request(
%     %       get, json, ?APP_LIMITS_URL("help,users,search,statuses"),
%     %       [200], Client2),
%     % io:format("Limits: ~p~n", [Limits]),

%     % ok.

%     ibrowse:send_req(
%         "https://stream.twitter.com/1.1/statuses/sample.json",
%         [{
%             "Authorization",
%             "OAuth oauth_consumer_key=\"ZPeKV4XvbjY8DiOnQJSQPeQVn\", oauth_nonce=\"c51bbb2600d108a7442a473220e04c43\", oauth_signature=\"k6fqVhhWbZlZyoTAUq6MtUE%2FVBM%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1466887904\", oauth_token=\"101055484-iQg2OaLNtEHInKDPX2Ca6MrCBkBZ74G94iHAPgJm\", oauth_version=\"1.0\""
%         }],
%         get
%     ).

%     % curl --get 'https://stream.twitter.com/1.1/statuses/sample.json' --header 'Authorization: OAuth oauth_consumer_key="hJY9CmIS9ja3CsOvKyQov9OB5", oauth_nonce="b65096ab317e35ee93c4244abd3c82d6", oauth_signature="%2BXVTr5AiJQ1F3K0qZT9MFnqVxJ8%3D", oauth_signature_method="HMAC-SHA1", oauth_timestamp="1466883341", oauth_token="101055484-PfPLXpERGmZhSKzQJYg1yzamdqJ7fpF6y6EkwhXi", oauth_version="1.0"' --verbose


-define(CONSUMER_SECRET, <<"7jGqYshgOycnwSpwXZP9UCOHQF1TSLAUa1YUeNBzM6NxzUbbCK">>).
-define(CONSUMER_KEY, <<"Yx6bsW73g6rFiKNDoc6sBNdeu">>).

-define(OAUTH2_TOKEN_URL, <<"https://api.twitter.com/oauth2/token">>).

-define(USER_TIMELINE_URL(User, StrCount),
        <<"https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
          , User, "&count=", StrCount>>).

-define(APP_LIMITS_URL(Resources),
        << "https://api.twitter.com/1.1/application/rate_limit_status.json?resources="
           , Resources>>).
run() ->
    application:ensure_all_started(oauth2c),
    application:ensure_all_started(ssl),
    {ok, _Headers, Client} =
        oauth2c:retrieve_access_token(
          <<"client_credentials">>, ?OAUTH2_TOKEN_URL, ?CONSUMER_KEY,
          ?CONSUMER_SECRET),
    {{ok, _Status1, _Headers1, Tweets}, Client2} =
        oauth2c:request(
          get, json, ?USER_TIMELINE_URL("twitterapi", "4"), [200], Client),
    io:format("Tweets: ~p~n", [Tweets]),
    {{ok, _Status2, _Headers2, Limits}, _Client3} =
        oauth2c:request(
          get, json, ?APP_LIMITS_URL("help,users,search,statuses"),
          [200], Client2),
    io:format("Limits: ~p~n", [Limits]),
    ok.