-module(reporter).
-export([start/1, report_move/4]).

% for some reason I have to open the file in the processes, can't open it then pass it in
starter(Filename) ->
    case file:open(Filename, [write, raw, {delayed_write, 4*1024*1024, 5000}]) of
        {ok, Fd} ->
            file:write(Fd, "time_megasecs,time_secs,time_microsecs,ant_id,cell_x,cell_y"),
            loop(Fd);
        {error, Reason} -> {error, Reason}
    end.

loop(Fd) ->
    receive
        {move, [Time, AntId, CellId]} ->
            {MegaSecs, Secs, MicroSecs} = Time,
            {CellX, CellY} = CellId,
            Out_string = io_lib:format("~p,~p,~p,~p,~p,~p~n",
                                       [MegaSecs, Secs, MicroSecs, AntId, CellX, CellY]),
            file:write(Fd, [Out_string]),
            loop(Fd);

        stop -> file:close(Fd)
    end.

%% public api
start(Filename) -> spawn(fun () -> starter(Filename) end).

report_move(Reporter, Time, AntId, CellId) ->
    Reporter ! {move, [Time, AntId, CellId]}.
