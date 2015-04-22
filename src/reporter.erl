-module(reporter).
-export([start/1, report_move/4, report_cell_change/4, stop/1]).

% for some reason I have to open the file in the processes, can't open it then pass it in
starter(Filename) ->
    A = file:open(Filename, [write, raw, {delayed_write, 4*1024*1024, 5000}]),
    case A of
        {ok, Fd} ->
            loop(Fd);
        {error, Reason} -> {error, Reason}
    end.

report(Doc, Fd) ->
    Out_string = jiffy:encode(Doc),
    file:write(Fd, [Out_string]),
    file:write(Fd, ["\n"]).

loop(Fd) ->
    receive
        {move, [Time, AntId, CellId]} ->
            {MegaSecs, Secs, MicroSecs} = Time,
            {CellX, CellY} = CellId,
            Doc = {[
              {time, {
                 [
                  {megasecs, MegaSecs},
                  {secs, Secs},
                  {microsecs, MicroSecs}
                 ]
              }},
              {antmove, {
                 [
                  {antid, AntId},
                  {cell, {[
                           {x, CellX},
                           {y, CellY}
                          ]}}
                 ]
              }}
            ]},
            report(Doc, Fd),
            loop(Fd);

        {weight_change, [Time, CellId, NewWeight]} ->
            {MegaSecs, Secs, MicroSecs} = Time,
            {CellX, CellY} = CellId,
            Doc = {[
              {time, {
                 [
                  {megasecs, MegaSecs},
                  {secs, Secs},
                  {microsecs, MicroSecs}
                 ]
              }},
              {weight_change, {
                 [
                  {cell, {[
                           {x, CellX},
                           {y, CellY}
                          ]}},
                  {new_weight, NewWeight}
                 ]
              }}
            ]},
            report(Doc, Fd),
            loop(Fd);

        {stop, ToWho} ->
            file:close(Fd),
            ToWho ! stopped
    end.

%% public api
start(Filename) ->
    spawn(fun () -> starter(Filename) end).

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
