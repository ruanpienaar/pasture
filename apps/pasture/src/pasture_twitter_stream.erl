-module (pasture_twitter_stream).
-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {python_pid}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

init({}) ->
    {ok, P} =
        python:start([{python_path, "/Users/rp/code/pasture"},
                      {python, "python2"}]),
    _TREF = erlang:start_timer(2, self(), start),
    {ok, #state{python_pid = P}}.

handle_call(Request, _From, State) ->
    io:format("Handle_call ~p \n",[Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Handle_cast ~p \n",[Msg]),
    {noreply, State}.

handle_info({timeout,_,start}, #state{python_pid = P } = State) ->
    python:call(P, 'tweepy_twitter_stream' ,'start', ["Boris Nemtsov"]),
    {noreply, State};
handle_info(Info, State) ->

    % Ex below:

    io:format("Handle_info ~p \n",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminate ~p \n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Example:

% "{\"created_at\":\"Sat Feb 28 23:28:31 +0000 2015\",\"id\":571814234222436352,\"id_str\":\"571814234222436352\",\"text\":\"RT @BrankoMilan: Good article.  \\\"After Boris Nemtsov\\u2019s Assassination, \\u2018There Are No Longer Any Limits\\\" http:\\/\\/t.co\\/yyZl57XGY8\",\"source\":\"\\u003ca href=\\\"http:\\/\\/twitter.com\\\" rel=\\\"nofollow\\\"\\u003eTwitter Web Client\\u003c\\/a\\u003e\",\"truncated\":false,\"in_reply_to_status_id\":null,\"in_reply_to_status_id_str\":null,\"in_reply_to_user_id\":null,\"in_reply_to_user_id_str\":null,\"in_reply_to_screen_name\":null,\"user\":{\"id\":1155756379,\"id_str\":\"1155756379\",\"name\":\"Adrian Florea\",\"screen_name\":\"adrianflorea13\",\"location\":\"Greensboro, NC\",\"url\":\"http:\\/\\/pages.iu.edu\\/~florea\",\"description\":\"Lecturer @UNCG. Ph.D. @IndianaUniv. Civil war. Separatism. Rebel governance. War and state making. Data. Causal mechanisms. RT=just that and nothing more.\",\"protected\":false,\"verified\":false,\"followers_count\":224,\"friends_count\":487,\"listed_count\":8,\"favourites_count\":694,\"statuses_count\":1552,\"created_at\":\"Thu Feb 07 02:24:07 +0000 2013\",\"utc_offset\":null,\"time_zone\":null,\"geo_enabled\":false,\"lang\":\"en\",\"contributors_enabled\":false,\"is_translator\":false,\"profile_background_color\":\"C0DEED\",\"profile_background_image_url\":\"http:\\/\\/abs.twimg.com\\/images\\/themes\\/theme1\\/bg.png\",\"profile_background_image_url_https\":\"https:\\/\\/abs.twimg.com\\/images\\/themes\\/theme1\\/bg.png\",\"profile_background_tile\":false,\"profile_link_color\":\"0084B4\",\"profile_sidebar_border_color\":\"C0DEED\",\"profile_sidebar_fill_color\":\"DDEEF6\",\"profile_text_color\":\"333333\",\"profile_use_background_image\":true,\"profile_image_url\":\"http:\\/\\/pbs.twimg.com\\/profile_images\\/524747067004956673\\/w9PnsiMc_normal.jpeg\",\"profile_image_url_https\":\"https:\\/\\/pbs.twimg.com\\/profile_images\\/524747067004956673\\/w9PnsiMc_normal.jpeg\",\"profile_banner_url\":\"https:\\/\\/pbs.twimg.com\\/profile_banners\\/1155756379\\/1393382502\",\"default_profile\":true,\"default_profile_image\":false,\"following\":null,\"follow_request_sent\":null,\"notifications\":null},\"geo\":null,\"coordinates\":null,\"place\":null,\"contributors\":null,\"retweeted_status\":{\"created_at\":\"Sat Feb 28 23:04:00 +0000 2015\",\"id\":571808063570755584,\"id_str\":\"571808063570755584\",\"text\":\"Good article.  \\\"After Boris Nemtsov\\u2019s Assassination, \\u2018There Are No Longer Any Limits\\\" http:\\/\\/t.co\\/yyZl57XGY8\",\"source\":\"\\u003ca href=\\\"http:\\/\\/twitter.com\\\" rel=\\\"nofollow\\\"\\u003eTwitter Web Client\\u003c\\/a\\u003e\",\"truncated\":false,\"in_reply_to_status_id\":null,\"in_reply_to_status_id_str\":null,\"in_reply_to_user_id\":null,\"in_reply_to_user_id_str\":null,\"in_reply_to_screen_name\":null,\"user\":{\"id\":990009265,\"id_str\":\"990009265\",\"name\":\"Branko Milanovic\",\"screen_name\":\"BrankoMilan\",\"location\":\"CUNY, New York\",\"url\":\"http:\\/\\/econ.worldbank.org\\/projects\\/inequality\",\"description\":\"1) Income inequality; 2) Politics;  3) History; 4) Soccer. Author of The Haves and the Have-nots: A brief and idiosyncratic history of global inequality.\",\"protected\":false,\"verified\":false,\"followers_count\":14536,\"friends_count\":994,\"listed_count\":550,\"favourites_count\":8660,\"statuses_count\":16518,\"created_at\":\"Wed Dec 05 02:15:33 +0000 2012\",\"utc_offset\":null,\"time_zone\":null,\"geo_enabled\":false,\"lang\":\"en\",\"contributors_enabled\":false,\"is_translator\":false,\"profile_background_color\":\"C0DEED\",\"profile_background_image_url\":\"http:\\/\\/abs.twimg.com\\/images\\/themes\\/theme1\\/bg.png\",\"profile_background_image_url_https\":\"https:\\/\\/abs.twimg.com\\/images\\/themes\\/theme1\\/bg.png\",\"profile_background_tile\":false,\"profile_link_color\":\"0084B4\",\"profile_sidebar_border_color\":\"C0DEED\",\"profile_sidebar_fill_color\":\"DDEEF6\",\"profile_text_color\":\"333333\",\"profile_use_background_image\":true,\"profile_image_url\":\"http:\\/\\/pbs.twimg.com\\/profile_images\\/470694288272003072\\/syIrT1QB_normal.jpeg\",\"profile_image_url_https\":\"https:\\/\\/pbs.twimg.com\\/profile_images\\/470694288272003072\\/syIrT1QB_normal.jpeg\",\"default_profile\":true,\"default_profile_image\":false,\"following\":null,\"follow_request_sent\":null,\"notifications\":null},\"geo\":null,\"coordinates\":null,\"place\":null,\"contributors\":null,\"retweet_count\":8,\"favorite_count\":6,\"entities\":{\"hashtags\":[],\"trends\":[],\"urls\":[{\"url\":\"http:\\/\\/t.co\\/yyZl57XGY8\",\"expanded_url\":\"http:\\/\\/nyti.ms\\/1LXfNEV\",\"display_url\":\"nyti.ms\\/1LXfNEV\",\"indices\":[86,108]}],\"user_mentions\":[],\"symbols\":[]},\"favorited\":false,\"retweeted\":false,\"possibly_sensitive\":false,\"filter_level\":\"low\",\"lang\":\"en\"},\"retweet_count\":0,\"favorite_count\":0,\"entities\":{\"hashtags\":[],\"trends\":[],\"urls\":[{\"url\":\"http:\\/\\/t.co\\/yyZl57XGY8\",\"expanded_url\":\"http:\\/\\/nyti.ms\\/1LXfNEV\",\"display_url\":\"nyti.ms\\/1LXfNEV\",\"indices\":[124,125]}],\"user_mentions\":[{\"screen_name\":\"BrankoMilan\",\"name\":\"Branko Milanovic\",\"id\":990009265,\"id_str\":\"990009265\",\"indices\":[3,15]}],\"symbols\":[]},\"favorited\":false,\"retweeted\":false,\"possibly_sensitive\":false,\"filter_level\":\"low\",\"lang\":\"en\",\"timestamp_ms\":\"1425166111890\"}\r\n"