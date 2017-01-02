-module(pasture_gtw).
-export([start_link/1,
         fetch_all/0,
         fetch/1]).

-include("pasture.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link({Id,Country}) ->
    gen_server:start_link({local, Country}, ?MODULE, {Id,Country}, []).

fetch_all() ->
    [ begin
        fetch(Country),
        timer:sleep(2000)
     end || {Id,Country} <- pasture_google_trends_sup:countries() ].

fetch(Country) ->
    gen_server:cast(Country, fetch).

init({Id,Country}) ->
    {ok, #{id=>Id, country=>Country}, 100}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(fetch, #{ id := Id } = State) ->
    do_fetch(Id),
    {noreply, State}.

handle_info(timeout, State) ->
    % fetch_all(),
    timer(),
    {noreply, State};
handle_info(timer, State) ->
    fetch_all(),
    timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

url(Id) ->
    "https://www.google.com/trends/hottrends/atom/feed?pn=p"++integer_to_list(Id).

timer() ->
    erlang:send_after(1000 * 60 * 60, self(), timer).

do_fetch(Id) ->
    Url = url(Id),
    {ok,"200", Headers, Xml} = ibrowse:send_req(Url, [], get),
    {ok, RssElement, Tail} = erlsom:simple_form(Xml),
    {RssTag, RssAttr, RssCont} = RssElement,
    [{"channel",_,ChannelContent}] = RssCont,
    Items = proplists:lookup_all("item", ChannelContent),
    lists:foreach(fun({"item", _, ItemContent}) ->
        ?WARNING("."),
        {"title",_,[Title]} = lists:keyfind("title", 1, ItemContent),
        ApproxCol = "{http://www.google.com/trends/hottrends}approx_traffic",
        {ApproxCol,_,[ApproxTraffic]} = lists:keyfind(ApproxCol, 1, ItemContent),
        {"pubDate",_,[PubDate]} = lists:keyfind("pubDate", 1, ItemContent),
        PictureCol = "{http://www.google.com/trends/hottrends}picture",
        PictureUrl =
            case lists:keyfind(PictureCol, 1, ItemContent) of
                {PictureCol,_,[PictureUrl2]} ->
                    [$/,$/|PictureUrl3] = PictureUrl2,
                    PictureUrl3;
                _ ->
                    ""
            end,
        NewsItemCol = "{http://www.google.com/trends/hottrends}news_item",
        NewsItems = proplists:lookup_all(NewsItemCol, ItemContent),
        lists:foreach(fun({"{http://www.google.com/trends/hottrends}news_item",_,NewsItem}) ->
            NT="{http://www.google.com/trends/hottrends}news_item_title",
            NS="{http://www.google.com/trends/hottrends}news_item_snippet",
            NU="{http://www.google.com/trends/hottrends}news_item_url",
            {NT,_,[NTVal]} = lists:keyfind(NT, 1, NewsItem),
            {NS,_,[NSVal]} = lists:keyfind(NS, 1, NewsItem),
            {NU,_,[NUVal]} = lists:keyfind(NU, 1, NewsItem),
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
        })
    end, Items).

% {"item",[],
%     [
%         {"title",[],["black eyed peas"]},
%         {"{http://www.google.com/trends/hottrends}approx_traffic",[],["20,000+"]},
%         {"description",[],[]},
%         {"link",[],["https://www.google.com/trends/hottrends?pn=p1#a=20170101-black+eyed+peas"]},
%         {"pubDate",[],["Sun, 01 Jan 2017 00:00:00 -0800"]},
%         {"{http://www.google.com/trends/hottrends}picture",[],["//t1.gstatic.com/images?q=tbn:ANd9GcRnlTTF8zNIc2l6pgIE09mWNhv7LAvqBR_PTTLr3UhJldXGy3S7_Guw7JI6x6erIDk1UzhQH-iw"]},
%         {"{http://www.google.com/trends/hottrends}picture_source",[],["International Business Times"]},
%         {"{http://www.google.com/trends/hottrends}news_item",[],
%             [
%               {"{http://www.google.com/trends/hottrends}news_item_title",[],["How To Make <b>Black</b>-<b>Eyed Peas</b>: An Easy New Year&#39;s Recipe In The Southern Tradition"]},
%               {"{http://www.google.com/trends/hottrends}news_item_snippet",[],["If you live anywhere near the Southern United States, you know it&#39;s lucky to have <b>black eyed</b>-<b>peas</b> for dinner on New Year&#39;s Day. Like &#39;em or not, it&#39;s a tradition. &quot;<b>Black</b>-<b>eyed peas</b>, collard green, pork chop and corn bread,&quot; Alabama resident Marcerlene&nbsp;..."]},
%               {"{http://www.google.com/trends/hottrends}news_item_url",[],["http://www.ibtimes.com/how-make-black-eyed-peas-easy-new-years-recipe-southern-tradition-2467849"]},
%               {"{http://www.google.com/trends/hottrends}news_item_source",[],["International Business Times"]}
%             ]
%         },
%         {"{http://www.google.com/trends/hottrends}news_item",[],
%             [
%               {"{http://www.google.com/trends/hottrends}news_item_title",[], ["Why do we eat <b>black</b>-<b>eyed peas</b>, hog jowls and greens on New Year&#39;s Day?"]},
%               {"{http://www.google.com/trends/hottrends}news_item_snippet",[], ["If you want wealth and happiness in 2017, you need to make sure you dine on <b>black eyed peas</b>, hog jowls and collard greens on Jan. 1. The <b>black</b>-<b>eyed</b>-<b>pea</b>, pork and greens tradition is found mostly in the South, though it&#39;s spread throughout the country <b>...</b>"]},
%               {"{http://www.google.com/trends/hottrends}news_item_url",[], ["http://www.al.com/news/index.ssf/2016/12/why_do_we_eat_black-eyed_peas.html"]},
%               {"{http://www.google.com/trends/hottrends}news_item_source",[], ["AL.com"]}
%             ]
%         }
%     ]
% }