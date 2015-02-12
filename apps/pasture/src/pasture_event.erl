-module (pasture_event).

-export([create_table/1,
         new/1,
         to_json/1]).

-include("../include/pasture.hrl").

-export([ init/3,
          rest_init/2,
          terminate/3 ]).

-export([handle_json/2]).

-define(STATE,pasture_event_state).
-record(?STATE,{ event_id }).

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
            {error,{C,E}}
    end.

new(Objs) ->
        #pasture_event{
            event_id   = pasture_utils:try_get_column(Objs,<<"event_id">>),
            event_name = pasture_utils:try_get_column(Objs,<<"event_name">>),
            event_url  = pasture_utils:try_get_column(Objs,<<"event_url">>),
            time       = pasture_utils:try_get_column(Objs,<<"time">>)
        }.

to_json(Rec) ->
    Fields = record_info(fields, ?MODULE),
    ListRec = tuple_to_list(Rec),
    PropList = lists:zip(Fields,ListRec),
    jsx:encode(PropList).

%% Rest

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {EventId,Req1} = cowboy_req:binding(event_id, Req),
    {ok, Req1, #?STATE{ event_id = EventId } }.

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

do_handle_json_path(Req,#?STATE{event_id = undefined} = State,
                    <<"GET">>, <<"/pasture_event">>) ->
    First = mnesia:dirty_first(pasture_event),
    FirstId = binary_to_list(First),
    {{true,"event/"++FirstId},Req,State};
do_handle_json_path(Req,#?STATE{event_id = EId} = State,
                    <<"GET">>, <<"/pasture_event/next">>)
        when EId =/= undefined ->
    Next = mnesia:dirty_next(pasture_event,EId),
    NextId = binary_to_list(Next),
    {{true,"event/"++NextId},Req,State};
do_handle_json_path(Req,#?STATE{event_id = EId} = State,
                    <<"GET">>, _) ->
    [Rec] = mnesia:dirty_read(pasture_event, EId),
    Json = to_json(Rec),
    {Json,Req,State};
do_handle_json_path(Req,State,<<"POST">>,_) ->
    SetBodyReq = cowboy_req:set_resp_body(<<"SomeBody!">>, Req),
    { {true,"path/to/resource"}, SetBodyReq, State}.

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