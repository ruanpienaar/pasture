-module (pasture_venue).

-export([create_table/1,
         new/1,
         to_json/1]).

-include("../include/pasture.hrl").

-export([ init/3,
          rest_init/2,
          terminate/3,
          resource_exists/2,
          handle_json/2]).

-define(STATE,pasture_venue_state).
-record(?STATE,{ venue_id,
                 venue }).

%% All available REST handler exports:
 -export([
%     allowed_methods/2        %% Methods : all
%     allow_missing_post/2     %% Methods : POST
%     charsets_provided/2      %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     content_types_accepted/2 %% Methods : POST, PUT, PATCH
    content_types_provided/2 %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     delete_completed/2       %% Methods : DELETE
%     delete_resource/2        %% Methods : DELETE
%     expires/2                %% Methods : GET, HEAD
%     forbidden/2              %% Methods : all
%     generate_etag/2          %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     is_authorized/2          %% Methods : all
%     is_conflict/2            %% Methods : PUT
%     known_methods/2          %% Methods : all
%     languages_provided/2     %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     last_modified/2          %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     malformed_request/2      %% Methods : all
%     moved_permanently/2      %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     moved_temporarily/2      %% Methods : GET, HEAD, POST, PATCH, DELETE
%     multiple_choices/2       %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     options/2                %% Methods : OPTIONS
%     previously_existed/2     %% Methods : GET, HEAD, POST, PATCH, DELETE
%     resource_exists/2        %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
%     service_available/2      %% Methods : all
%     uri_too_long/2           %% Methods : all
%     valid_content_headers/2  %% Methods : all
%     valid_entity_length/2    %% Methods : all
%     variances/2              %% Methods : GET, HEAD, POST, PUT, PATCH, DELETE
 ]).


create_table(Nodes) ->
    try
        mnesia:table_info(?MODULE,attributes)
    catch
        exit:{aborted,{no_exists,?MODULE,attributes}} ->
            {atomic,ok} =
                mnesia:create_table(
                        ?MODULE,
                        [{type,set},
                         {disc_only_copies,Nodes},
                         {attributes,record_info(fields, ?MODULE)},
                         {majority, true}
                       ]);
        C:E ->
            throw({stop,[{c,C},{e,E}]})
    end.

new(Objs) ->
        #pasture_venue{
            lat        = pasture_utils:try_get_column(Objs,<<"lat">>),
            lon        = pasture_utils:try_get_column(Objs,<<"lon">>),
            venue_id   = pasture_utils:try_get_column(Objs,<<"venue_id">>),
            venue_name = pasture_utils:try_get_column(Objs,<<"venue_name">>)
        }.


to_prop(Rec) ->
    Fields = record_info(fields, pasture_venue),
    ListRec = tuple_to_list(Rec) -- [?MODULE],
    lists:zip(Fields,ListRec).

to_json(Rec) ->
    Prop = to_prop(Rec),
    jsx:encode(Prop).

%% -----------------------------------
%% Rest

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {VenueId,Req1} = case cowboy_req:binding(venue_id, Req) of
        {undefined, R} ->
            {undefined, R};
        {VID,R} ->
            {binary_to_integer(VID),R}
    end,
    {ok, Req1, #?STATE{ venue_id = VenueId } }.

resource_exists(Req,#?STATE{ venue_id = undefined } = State) ->
    {Path,Req1} = cowboy_req:path(Req),
    case Path of
        <<"/pasture_venue">> ->
            {true,Req1,State};
        <<"/pasture_venue/">> ->
            {true,Req1,State};
        _ ->
            {false,Req1,State}
    end;
resource_exists(Req,#?STATE{ venue_id = VId } = State) ->
    case mnesia:dirty_read(?MODULE,VId) of
        []    -> {false,Req,State};
        [Rec] -> {true,Req,State#?STATE{ venue = Rec }}
    end.

content_types_provided(Req, State) ->
    {[  %% {<<"text/html">>,       handle_html},
        %% {<<"text/plain">>,      handle_text},
        {<<"application/json">>,handle_json}
    ], Req, State}.

% handle_html(Req, State) ->
%     {Body,Req,State}.

% handle_text(Req, State) ->
%     {Body,Req,State}.

handle_json(Req, State) ->
    {Method,Req1} = cowboy_req:method(Req),
    {Path,Req2} = cowboy_req:path(Req1),
    do_handle_json_path(Req2,State,Method, Path).

do_handle_json_path(Req,#?STATE{venue_id = undefined} = State,
                    <<"GET">>, Path)
                    when Path =:= <<"/pasture_venue">>;
                         Path =:= <<"/pasture_venue/">> ->
    First = mnesia:dirty_first(pasture_venue),
    {json_range(first,First),Req,State};
%% resource_exists will handle a invalid venue_id ( First id )
do_handle_json_path(Req,#?STATE{venue = _E, venue_id = VId} = State,
                    <<"GET">>, _Path) ->
    {json_range(page,VId),Req,State}.

json_range(_,'$end_of_table') ->
    <<"{}">>;
json_range(first,First) ->
    ListFirst = integer_to_list(First),
    {Next,JsonRecs,_} = loop(next,First),
    ListNext = integer_to_list(Next),
    json_struct(ListFirst,ListNext,JsonRecs);
json_range(_,First) ->
    % ListFirst = integer_to_list(First),
    {Next,JsonRecs,_} = loop(next,First),
    ListNext = integer_to_list(Next),
    %% Find a nicer way of getting Prev....
    {Prev,JsonRecs,_} = loop(prev,First),
    ListPrev = integer_to_list(Prev),
    json_struct(ListPrev,ListNext,JsonRecs).

json_struct(ListPrev,ListNext,JsonRecs) ->
    jsx:encode([{prev,list_to_binary("/pasture_venue/"++ListPrev)},
                {next,list_to_binary("/pasture_venue/"++ListNext)},
                {pasture_venue,lists:reverse(JsonRecs)}
               ]).

loop(prev,First) ->
    loop_fun(fun(IdAcc) -> mnesia:dirty_prev(?MODULE,IdAcc) end,First);
loop(next,First) ->
    loop_fun(fun(IdAcc) -> mnesia:dirty_next(?MODULE,IdAcc) end,First).

loop_fun(F,First) ->
    lists:foldl(fun(_,{IdAcc,RecsAcc,false}) ->
                {IdAcc,RecsAcc,false};
                       (_,{IdAcc,RecsAcc,true}) ->
            case mnesia:dirty_read(?MODULE,IdAcc) of
                [] ->
                    {IdAcc,
                     RecsAcc, false};
                [Rec] ->
                    {F(IdAcc),
                     [to_prop(Rec)|RecsAcc], true}
            end
        end, {First,[],true}, lists:seq(1,100)).

% do_handle_json_path(Req,State,<<"POST">>,_) ->
%     SetBodyReq = cowboy_req:set_resp_body(<<"SomeBody!">>, Req),
%     { {true,"path/to/resource"}, SetBodyReq, State}.

terminate(normal, _Req, _State) ->
    ok;
terminate({crash, _Class, _Reason}, _Req, _State) ->
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

%----------------------

% allowed_methods(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% allow_missing_post(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% charsets_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% content_types_accepted(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% content_types_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% delete_completed(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% delete_resource(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% expires(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% forbidden(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% generate_etag(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% is_authorized(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% is_conflict(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% known_methods(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% languages_provided(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% last_modified(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% malformed_request(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% moved_permanently(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% moved_temporarily(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% multiple_choices(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% options(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% previously_existed(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% resource_exists(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% service_available(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% uri_too_long(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% valid_content_headers(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% valid_entity_length(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}
% variances(Req,State) ->
%     {Value, Req, State} | {halt, Req, State}