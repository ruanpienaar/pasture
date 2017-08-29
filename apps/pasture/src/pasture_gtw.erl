-module(pasture_gtw).
-export([start_link/1,
         do_fetch/1]).

-include("pasture.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link({Id,Country}) ->
    gen_server:start_link({local, Country}, ?MODULE, {Id,Country}, []).

init({Id,Country}) ->
    process_flag(trap_exit, true),
    {ok, #{id=>Id, country=>Country, offset=>Id*10000}, 0}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(timeout, #{ country := Country, offset := Offset } = State) ->
    timer(Country, Offset),
    {noreply, State};
handle_info(timer, #{ country := Country, offset := Offset, id := Id } = State) ->
    WorkerPid = spawn_link(fun() -> do_fetch(Id) end),
    timer(Country, Offset),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #{ worker_pid := Pid } = State) ->
    {noreply, State#{ worker_pid => undefined }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

url(Id) ->
    "https://trends.google.com/trends/hottrends/atom/feed?pn=p"++integer_to_list(Id).

timer(Country, Offset) ->
    timer:send_after((1000 * 60 * 60)-Offset, Country, timer).

do_fetch(Id) ->
    Url = url(Id),
    {ok,"200", _Headers, Xml} = ibrowse:send_req(Url, [], get),
    {ok, {_RssTag, _RssAttr, [{"channel",_,ChannelContent}]}, _Tail} = erlsom:simple_form(Xml),
    lists:foreach(fun({"item", _, ItemContent}) ->
        handle_item(Id, ItemContent)
    end, proplists:lookup_all("item", ChannelContent)).

handle_item(Id, ItemContent) ->
    {"title",_,[Title]} = lists:keyfind("title", 1, ItemContent),
    ApproxCol = "{https://trends.google.com/trends/hottrends}approx_traffic",
    {ApproxCol,_,[ApproxTraffic]} = lists:keyfind(ApproxCol, 1, ItemContent),
    {"pubDate",_,[PubDate]} = lists:keyfind("pubDate", 1, ItemContent),
    PictureCol = "{https://trends.google.com/trends/hottrends}picture",
    PictureUrl =
        case lists:keyfind(PictureCol, 1, ItemContent) of
            {PictureCol,_,[PictureUrl2]} ->
                [$/,$/|PictureUrl3] = PictureUrl2,
                PictureUrl3;
            _ ->
                ""
        end,
    NewsItemCol = "{https://trends.google.com/trends/hottrends}news_item",
    NewsItems = proplists:lookup_all(NewsItemCol, ItemContent),
    lists:foreach(fun({"{https://trends.google.com/trends/hottrends}news_item",_,NewsItem}) ->
        {"{https://trends.google.com/trends/hottrends}news_item_title",_,[NTVal]} = lists:keyfind("{https://trends.google.com/trends/hottrends}news_item_title", 1, NewsItem),
        {"{https://trends.google.com/trends/hottrends}news_item_snippet",_,[NSVal]} = lists:keyfind("{https://trends.google.com/trends/hottrends}news_item_snippet", 1, NewsItem),
        {"{https://trends.google.com/trends/hottrends}news_item_url",_,[NUVal]} = lists:keyfind("{https://trends.google.com/trends/hottrends}news_item_url", 1, NewsItem),
        ok = pasture_db_esqlite:add(#pasture_google_trend_news_item{
            country_id = Id,
            title=Title,
            pub_date=PubDate,
            news_item_title=NTVal,
            news_item_snippet=NSVal,
            news_item_source=NUVal
        })
    end, NewsItems),
    ok = pasture_db_esqlite:add(#pasture_google_trend{
            country_id = Id,
            title = Title,
            approx_traffic = ApproxTraffic,
            pub_date = PubDate,
            picture_url = PictureUrl
    }).

% {"item",[],
%     [
%         {"title",[],["black eyed peas"]},
%         {"{http://trends.google.com/trends/hottrends}approx_traffic",[],["20,000+"]},
%         {"description",[],[]},
%         {"link",[],["https://www.google.com/trends/hottrends?pn=p1#a=20170101-black+eyed+peas"]},
%         {"pubDate",[],["Sun, 01 Jan 2017 00:00:00 -0800"]},
%         {"{http://trends.google.com/trends/hottrends}picture",[],["//t1.gstatic.com/images?q=tbn:ANd9GcRnlTTF8zNIc2l6pgIE09mWNhv7LAvqBR_PTTLr3UhJldXGy3S7_Guw7JI6x6erIDk1UzhQH-iw"]},
%         {"{http://trends.google.com/trends/hottrends}picture_source",[],["International Business Times"]},
%         {"{http://trends.google.com/trends/hottrends}news_item",[],
%             [
%               {"{http://trends.google.com/trends/hottrends}news_item_title",[],["How To Make <b>Black</b>-<b>Eyed Peas</b>: An Easy New Year&#39;s Recipe In The Southern Tradition"]},
%               {"{http://trends.google.com/trends/hottrends}news_item_snippet",[],["If you live anywhere near the Southern United States, you know it&#39;s lucky to have <b>black eyed</b>-<b>peas</b> for dinner on New Year&#39;s Day. Like &#39;em or not, it&#39;s a tradition. &quot;<b>Black</b>-<b>eyed peas</b>, collard green, pork chop and corn bread,&quot; Alabama resident Marcerlene&nbsp;..."]},
%               {"{http://trends.google.com/trends/hottrends}news_item_url",[],["http://www.ibtimes.com/how-make-black-eyed-peas-easy-new-years-recipe-southern-tradition-2467849"]},
%               {"{http://trends.google.com/trends/hottrends}news_item_source",[],["International Business Times"]}
%             ]
%         },
%         {"{http://trends.google.com/trends/hottrends}news_item",[],
%             [
%               {"{http://trends.google.com/trends/hottrends}news_item_title",[], ["Why do we eat <b>black</b>-<b>eyed peas</b>, hog jowls and greens on New Year&#39;s Day?"]},
%               {"{http://trends.google.com/trends/hottrends}news_item_snippet",[], ["If you want wealth and happiness in 2017, you need to make sure you dine on <b>black eyed peas</b>, hog jowls and collard greens on Jan. 1. The <b>black</b>-<b>eyed</b>-<b>pea</b>, pork and greens tradition is found mostly in the South, though it&#39;s spread throughout the country <b>...</b>"]},
%               {"{http://trends.google.com/trends/hottrends}news_item_url",[], ["http://www.al.com/news/index.ssf/2016/12/why_do_we_eat_black-eyed_peas.html"]},
%               {"{http://trends.google.com/trends/hottrends}news_item_source",[], ["AL.com"]}
%             ]
%         }
%     ]
% }