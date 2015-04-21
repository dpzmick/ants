-module(reporter).
-export([start/2, report_move/4, report_cell_change/4, stop/1]).

% for some reason I have to open the file in the processes, can't open it then pass it in
starter(Filename_ant, Filename_cell) ->
    A = file:open(Filename_ant, [write, raw, {delayed_write, 4*1024*1024, 5000}]),
    B = file:open(Filename_cell, [write, raw, {delayed_write, 4*1024*1024, 5000}]),
    case {A,B} of
        {{ok, Fd_ant}, {ok, Fd_cell}} ->
            file:write(Fd_ant, io_lib:format("time_megasecs,time_secs,time_microsecs,ant_id,cell_x,cell_y~n",[])),
            file:write(Fd_cell, io_lib:format("time_megasecs,time_secs,time_microsecs,cell_x,cell_y,new_weight~n",[])),
            loop(Fd_ant, Fd_cell);
        {error, Reason} -> {error, Reason}
    end.

loop(Fd_ant, Fd_cell) ->
    receive
        {move, [Time, AntId, CellId]} ->
            {MegaSecs, Secs, MicroSecs} = Time,
            {CellX, CellY} = CellId,
            Out_string = io_lib:format("~p,~p,~p,~p,~p,~p~n",
                                       [MegaSecs, Secs, MicroSecs, AntId, CellX, CellY]),
            file:write(Fd_ant, [Out_string]),
            loop(Fd_ant, Fd_cell);

        {weight_change, [Time, CellId, NewWeight]} ->
            {MegaSecs, Secs, MicroSecs} = Time,
            {CellX, CellY} = CellId,
            Out_string = io_lib:format("~p,~p,~p,~p,~p,~p~n",
                                       [MegaSecs, Secs, MicroSecs, CellX, CellY, NewWeight]),
            file:write(Fd_cell, [Out_string]),
            loop(Fd_ant, Fd_cell);

        {stop, ToWho} ->
            file:close(Fd_ant),
            file:close(Fd_cell),
            ToWho ! stopped
    end.

%% public api
start(Filename_ant, Filename_cell) ->
    spawn(fun () -> starter(Filename_ant, Filename_cell) end).

report_move(Reporter, Time, AntId, CellId) ->
    Reporter ! {move, [Time, AntId, CellId]}.

report_cell_change(Reporter, Time, CellId, NewWeight) ->
    Reporter ! {weight_change, [Time, CellId, NewWeight]}.

stop(undefined) -> ok;
stop(Reporter) ->
    Reporter ! {stop, self()},
    receive
        stopped -> ok
    end.
