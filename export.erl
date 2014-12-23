-module (export).
-export ([go/0]).

go() ->
    {ok,FD}= file:open("records.txt",[write]),
    First = rpc:call('pasture@192.168.1.80',
                        mnesia,dirty_first,[pasture_meetup]),
    loop(First,FD).

loop('$end_of_table',FD) ->
    file:close(FD);
loop(Key,FD) ->
    ok = file:write(FD,jsx:encode(Key)),
    loop(rpc:call('pasture@192.168.1.80',
                        mnesia,dirty_next,[pasture_meetup,Key]),FD).