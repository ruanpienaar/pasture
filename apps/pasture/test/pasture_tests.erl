-module(pasture_tests).

-define(NOTEST, true).
-define(NOASSERT, true).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            {"Test parsing of Chunked http responses",
                fun test_parse/0 }
        ]
    }.

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:

test_parse() ->

    %% Use cowboy to chunk to ibrowse.

    %% But for now just test parsing all types of responses, partial or not

    true.

setup() ->
    {ok,Pid} = pasture_meetup:start_link(),
    Pid.

teardown(Pid) ->
    exit(Pid,kill).