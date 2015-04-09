-module(rough).
-compile(export_all).

rough(Send, Id) ->
    case Send of
        undefined -> undefined;
        _ -> Send ! do_a_thing
    end,
    receive
        do_a_thing ->
            io:format("doing a thing, Id: ~p~n", [Id]),
            timer:sleep(1000),
            rough(self(), Id);
        stop ->
            io:format("stopping ~p~n", [Id])
    end.

rough(Id) -> rough(undefined, Id).

pound(Num) ->
    Pids = [spawn(rough, rough, [X]) || X <- lists:seq(1,Num)],
    lists:map(fun(E) -> E ! do_a_thing, E end, Pids).

stopall(Rs) -> lists:map(fun(E) -> E ! stop end, Rs).
