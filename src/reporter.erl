-module(reporter).
-export([start/0, report_move/4]).

% TODO start time

loop() ->
    receive
        {move, [Time, AntId, CellId]} ->
            io:format("[~p] Ant: ~p moved to cell: ~p~n", [Time, AntId, CellId]),
            loop()
    end.

%% public api
start() ->
    spawn(fun () -> loop() end).

report_move(Reporter, Time, AntId, CellId) ->
    Reporter ! {move, [Time, AntId, CellId]}.
