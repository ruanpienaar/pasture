-record(pasture_event,{id,
                       event_id,
                       event_name,
                       event_url,
                       time
                      }).

-record(pasture_group,{id,
                       group_city,
                       group_country,
                       group_id,
                       group_lat,
                       group_lon,
                       group_name,
                       group_state,
                       group_topics,
                       group_urlname
                      }).

-record(pasture_member,{id,
                        member_id,
                        member_name,
                        other_services,
                        photo
                       }).

-record(pasture_venue,{id,
                       lat,
                       lon,
                       venue_id,
                       venue_name
                      }).

-record(pasture_ids,{tbl,
                     id
                    }).

-define(DEBUG(Msg),
        lager:debug(Msg)).
-define(DEBUG(Msg, Args),
        lager:debug(Msg, Args)).

-define(INFO(Msg),
        lager:info(Msg)).
-define(INFO(Msg, Args),
        lager:info(Msg, Args)).

-define(WARNING(Msg),
        lager:warning(Msg)).
-define(WARNING(Msg, Args),
        lager:warning(Msg, Args)).

-define(ERROR(Msg),
        lager:error(Msg)).
-define(ERROR(Msg, Args),
        lager:error(Msg, Args)).