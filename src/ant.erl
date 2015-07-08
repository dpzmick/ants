-module(ant).
-export([start/2, wakeup_and_move/1, tell_neighbors/2, you_moved/2, failed_move/1, ant_id/1, stop/1]).

opposite_direction(away) -> towards;
opposite_direction(towards) -> away.

priv_do_weight_update(Reporter, Weights, CurrentCell) ->
    MaxNeighbors = lists:max(Weights),
    NewWeight = MaxNeighbors / 2,
    CurrentWeight = cell:cell_weight(CurrentCell),
    if
        MaxNeighbors > CurrentWeight, NewWeight > 1.0, CurrentWeight /= NewWeight ->
            reporter:report_cell_change(Reporter, os:timestamp(), cell:cell_id(CurrentCell), NewWeight),
            cell:set_weight(CurrentCell, NewWeight);
        true -> ok
    end.

priv_pick_neighbor(Cells, Probabilities) ->
    HackyWeightedList = lists:foldl(
                          fun({P,C}, Acc) -> [C || _ <- lists:seq(1, max(1, round(P*100)))] ++ Acc end,
                          [], lists:zip(Probabilities, Cells)),
    Index = random:uniform(length(HackyWeightedList)),
    lists:nth(Index, HackyWeightedList).

priv_distance_comp(Cells, StartingCell, away) ->
    Distances = lists:map(fun(Cell) -> cell:distance(Cell, StartingCell) end, Cells),
    DSum      = max(1, lists:sum(Distances)),
    NormDists = lists:map(fun(D) -> D / DSum end, Distances),
    NormDists;

priv_distance_comp(Cells, StartingCell, towards) ->
    Distances = lists:map(fun(Cell) -> cell:distance(Cell, StartingCell) end, Cells),
    DMax      = lists:max(Distances),
    DD        = lists:map(fun (D) -> DMax - D end, Distances),
    DSum      = max(1, lists:sum(DD)),
    NormDists = lists:map(fun(D) -> D / DSum end, DD),
    NormDists.

priv_got_neighbors(Neighbors, {_, CurrentCell, Reporter, StartingCell, Direction, DWeight, SWeight}) ->
    Cells = lists:map(fun({_,Cell}) -> Cell end, dict:to_list(Neighbors)),

    NormDists = priv_distance_comp(Cells, StartingCell, Direction),

    %% compute pheromone stuff
    Smells     = lists:map(fun(C) -> cell:cell_weight(C) end, Cells),
    SSum       = lists:sum(Smells),
    NormSmells = lists:map(fun(S) -> S / SSum end, Smells),

    %% compute cell move probability
    Zipped  = lists:zip(NormDists, NormSmells),
    Weights = lists:map(fun ({D, S}) -> DWeight*D + SWeight * S end, Zipped),
    Sum     = max(1, lists:sum(Weights)),
    Probs   = lists:map(fun (W) -> W / Sum end, Weights),

    Choice = priv_pick_neighbor(Cells, Probs),
    priv_do_weight_update(Reporter, Smells, CurrentCell),
    cell:move_ant_to(Choice, self()).

priv_statify(Id, CurrentCell, Reporter, StartingCell, Direction, DWeight, SWeight) ->
    {Id, CurrentCell, Reporter, StartingCell, Direction, DWeight, SWeight}.

priority_wrapper({Id, _, _, _, _, _, _}, Function) when is_number(Id) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, Function()
    after
        0 -> Function()
    end.

inner_loop_helper(Waiter, State = {Id, CurrentCell, Reporter, StartingCell, Direction, D, S})
  when is_pid(Waiter), is_number(Id) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, inner_loop_helper(Waiter, State);

        {neighbors, Neighbors} ->
            priv_got_neighbors(Neighbors, State),
            inner_loop(Waiter, State);

        {move_to, Cell} ->
            reporter:report_move(Reporter, os:timestamp(), Id, cell:cell_id(Cell)),
            cell:ant_leaving(CurrentCell, self()),
            Food = cell:has_food(Cell),
            Start = cell:eq(StartingCell, Cell),
            AtEdge = cell:at_edge(Cell),
            Waiter ! done,
            if
                %% catches the case where we are on edge but this is the first cell
                StartingCell == undefined ->
                    loop(priv_statify(Id, Cell, Reporter, Cell, Direction, D, S));
                true ->
                    if
                        Food ->
                            NewWeight = 1000.0,
                            reporter:report_cell_change(Reporter, os:timestamp(), cell:cell_id(Cell), NewWeight),
                            cell:set_weight(Cell, NewWeight),
                            loop(priv_statify(Id, Cell, Reporter, StartingCell, towards, 0.3, S));
                        Start ->
                            reporter:report_got_home(Reporter, os:timestamp(), Id),
                            loop(priv_statify(Id, Cell, Reporter, StartingCell, away, D, 0.0));
                        AtEdge ->
                            loop(priv_statify(Id, Cell, Reporter, StartingCell,
                                              opposite_direction(Direction), D, S));
                        true ->
                            loop(priv_statify(Id, Cell, Reporter, StartingCell, Direction, D, S))
                    end
            end;

        move_failed ->
            Waiter ! done,
            loop(State)
    end.

loop_helper(State = {Id, CurrentCell, _, _, _, _, _}) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, loop_helper(State);

        {wakeup_and_move, Waiter} ->
            cell:tell_neighbors(CurrentCell, self()),
            inner_loop(Waiter, State)

    end.

inner_loop(Waiter, State) -> priority_wrapper(State, fun () -> inner_loop_helper(Waiter, State) end).
loop(State) -> priority_wrapper(State, fun () -> loop_helper(State) end).

%% public api
start(Id, Reporter) when is_number(Id) ->
    spawn(fun () ->
                  {A,B,C} = erlang:now(),
                  random:seed(A, B, C + Id),
                  loop(priv_statify(Id, undefined, Reporter, undefined, away, 0.3, 1.0))
          end).

%% blocks
wakeup_and_move(Ant) when is_pid(Ant) ->
    Ant ! {wakeup_and_move, self()},
    receive
        done -> ok
    end.

%% idk how to check if is_dict
tell_neighbors(Ant, Neighbors) when is_pid(Ant) ->
    Ant ! {neighbors, Neighbors}.

you_moved(Ant, ToCell) when is_pid(Ant), is_pid(ToCell) ->
    Ant ! {move_to, ToCell}.

failed_move(Ant) when is_pid(Ant) ->
    Ant ! move_failed.

ant_id(Ant) when is_pid(Ant) ->
    Ant ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.

stop(undefined) -> ok;
stop(Ant) when is_pid(Ant) ->
    Ant ! {stop, self()},
    receive
        stopped -> ok
    end.
