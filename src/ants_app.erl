-module(ants_app).
-export([start/4, stop/1]).

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
    loop(Ant).

start(Xmax, Ymax, NumAnts, OutputDir) ->
    CellCoords = [{X,Y} || X <- lists:seq(1,Xmax), Y <- lists:seq(1,Ymax)],
    Cells = array:from_list([cell:start(Id) || Id <- CellCoords]),
    iter(Xmax, Ymax, Cells, 0, 0),

    Ants = lists:map(
      fun (X) ->
              Reporter = reporter:start(io_lib:format("~s/ant~p", [OutputDir, X])),
              io:format("Reporter: ~p~n", [Reporter]),
              A = ant:start(X, Reporter),
              cell:move_ant_to(array:get(X, Cells), A),
              {Reporter, A}
      end,
      lists:seq(1,NumAnts)),

    Loopers = lists:map(fun ({_, Ant}) ->
                                spawn(fun () -> loop(Ant) end)
                        end,
                        Ants),

    {Cells, Ants, Loopers}.

stop({Cells, Ants, Loopers}) ->
    lists:map(fun ({R, A}) -> ant:stop(A), reporter:stop(R) end, Ants),
    lists:map(fun (Looper) -> exit(Looper, kill) end, Loopers),
    array:map(fun (_, Cell) -> cell:stop(Cell) end, Cells).
