-module(ants_app).
-export([start/2, stop/1]).

xy_toIndex(Xmax, X, Y) -> X + Y*Xmax.

%% this does not please me
iter(Xmax, Ymax, Cells, Xcurr, Ycurr) ->
    Index = xy_toIndex(Xmax, Xcurr, Ycurr),
    CurrCell = array:get(Index, Cells),
    if
        Xcurr == Xmax - 1 ->
            if
                Ycurr == Ymax - 1-> ok;
                true -> iter(Xmax, Ymax, Cells, 0, Ycurr + 1)
            end;
        true ->
            RightX = Xcurr + 1,
            RightY = Ycurr,
            RightIndex = xy_toIndex(Xmax, RightX, RightY),
            RightCell = array:get(RightIndex, Cells),

            cell:register_neighbor(CurrCell, RightCell, e),

            if
                Ycurr == Ymax - 1 -> ok;
                true ->
                    LowerX = Xcurr,
                    LowerY = Ycurr + 1,
                    LowerIndex = xy_toIndex(Xmax, LowerX, LowerY),
                    LowerCell = array:get(LowerIndex, Cells),
                    cell:register_neighbor(CurrCell, LowerCell, s)
            end,
            iter(Xmax, Ymax, Cells, Xcurr + 1, Ycurr)
    end.

loop(Ant) ->
    ant:wakeup_and_move(Ant),
    timer:sleep(5),
    loop(Ant).

app(Xmax, Ymax, Ants) ->
    CellCoords = [{X,Y} || X <- lists:seq(1,Xmax), Y <- lists:seq(1,Ymax)],
    Cells = array:from_list([cell:start(Id) || Id <- CellCoords]),
    iter(Xmax, Ymax, Cells, 0, 0),
    lists:map(
      fun ({X, Ant}) -> cell:move_ant_to(array:get(X, Cells), Ant) end,
      lists:zip(lists:seq(1,100), Ants)),
    lists:map(fun (Ant) -> spawn(fun () -> loop(Ant) end) end, Ants).

start(Xmax, Ymax) ->
    Ants = [ant:start(X) || X <- lists:seq(1,100)],
    Pid = spawn(fun () -> app(Xmax, Ymax, Ants) end),
    Pid.

stop(Pid1) ->
    exit(Pid1, kill).
