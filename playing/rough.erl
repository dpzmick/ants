-module(rough).
-export([pound/2, stopall/1]).

rough(Send, Id, Delay) ->
    case Send of
        undefined -> undefined;
        _ -> Send ! do_a_thing
    end,
    receive
        do_a_thing ->
            timer:sleep(Delay),
            rough(self(), Id, Delay);
        stop -> ok
    end.

rough(Id, Delay) -> rough(undefined, Id, Delay).

pound(Num, Delay) ->
    Pids = [spawn(fun () -> rough(X, Delay) end) || X <- lists:seq(1,Num)],
    lists:map(fun(E) -> E ! do_a_thing, E end, Pids).

stopall(Rs) -> lists:map(fun(E) -> E ! stop end, Rs).
