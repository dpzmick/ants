-module(rough).
-export([rough/2, pound/1, stopall/1]).

rough(Send, Id, Delay) ->
    case Send of
        undefined -> undefined;
        _ -> Send ! do_a_thing
    end,
    receive
        do_a_thing ->
            io:format("doing a thing, Id: ~p~n", [Id]),
            timer:sleep(Delay),
            rough(self(), Id);
        stop ->
            io:format("stopping ~p~n", [Id])
    end.

rough(Id, Delay) -> rough(undefined, Id, Delay).

pound(Num) ->
    Pids = [spawn(rough, rough, [X]) || X <- lists:seq(1,Num)],
    lists:map(fun(E) -> E ! do_a_thing, E end, Pids).

stopall(Rs) -> lists:map(fun(E) -> E ! stop end, Rs).
