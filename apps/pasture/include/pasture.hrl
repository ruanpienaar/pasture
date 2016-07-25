-record(pasture_event,{event_id,
                       event_name,
                       event_url,
                       time
                      }).

-record(pasture_group,{group_id,
                       group_city,
                       group_country,
                       group_lat,
                       group_lon,
                       group_name,
                       group_state,
                       group_topics,
                       group_urlname
                      }).

-record(pasture_member,{member_id,
                        member_name,
                        other_services,
                        photo
                       }).

-record(pasture_venue,{venue_id,
                       lat,
                       lon,
                       venue_name
                      }).

-record(pasture_twitter,{id,
                         filter_str,
                         json
                        }).

-record(pasture_twitter_search,{search_str,
                                count
                               }).

-record(pasture_twitter_user,{id
                              }).
-record(pasture_twitter_location,{location,
                                  count
                                  }).
-record(pasture_twitter_tweet_string,{string}).

-define(DEBUG(Msg),lager:debug(Msg)).
-define(DEBUG(Msg, Args),lager:debug(Msg, Args)).

-define(INFO(Msg),lager:info(Msg)).
-define(INFO(Msg, Args),lager:info(Msg, Args)).

-define(NOTICE(Msg),lager:notice(Msg)).
-define(NOTICE(Msg, Args),lager:notice(Msg, Args)).

-define(WARNING(Msg),lager:warning(Msg)).
-define(WARNING(Msg, Args),lager:warning(Msg, Args)).

-define(ERROR(Msg),lager:error(Msg)).
-define(ERROR(Msg, Args),lager:error(Msg, Args)).

-define(CRITICAL(Msg),lager:critical(Msg)).
-define(CRITICAL(Msg, Args),lager:critical(Msg, Args)).

-define(ALERT(Msg),lager:alert(Msg)).
-define(ALERT(Msg, Args),lager:alert(Msg, Args)).

-define(EMERGENCY(Msg),lager:emergency(Msg)).
-define(EMERGENCY(Msg, Args),lager:emergency(Msg, Args)).