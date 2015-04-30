-module(ant).
-export([start/2, wakeup_and_move/1, tell_neighbors/2, you_moved/2, failed_move/1, ant_id/1, stop/1]).

%% pick one of these randomly (using their weight) and try to move to it
priv_pick_neighbor(Reporter, Neighbors, CurrentCell) ->
    Cells = lists:map(fun({_,Cell}) -> Cell end, dict:to_list(Neighbors)),
    Weights = lists:map(fun(C) -> cell:cell_weight(C) end, Cells),
    Sum = lists:sum(Weights),
    Probabilities = lists:map(fun(W) -> W / Sum end, Weights),
    HackyWeightedList = lists:foldl(
                          fun({P,C}, Acc) -> [C || _ <- lists:seq(1, round(P*100))] ++ Acc end,
                          [], lists:zip(Probabilities, Cells)),
    Index = random:uniform(length(HackyWeightedList)),
    MaxNeighbors = lists:max(Weights),
    NewWeight = MaxNeighbors / 2,
    CurrentWeight = cell:cell_weight(CurrentCell),
    if
        MaxNeighbors > CurrentWeight, NewWeight > 1.0, CurrentWeight /= NewWeight ->
            reporter:report_cell_change(Reporter, os:timestamp(), cell:cell_id(CurrentCell), NewWeight),
            cell:set_weight(CurrentCell, NewWeight);
        true -> ok
    end,
    lists:nth(Index, HackyWeightedList).

priv_got_neighbors(Reporter, Neighbors, CurrentCell) ->
    Choice = priv_pick_neighbor(Reporter, Neighbors, CurrentCell),
    cell:move_ant_to(Choice, self()).

priv_statify(Id, CurrentCell, Reporter, StartingCell) -> {Id, CurrentCell, Reporter, StartingCell}.

priority_wrapper({Id, _, _}, Function) when is_number(Id) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, Function()
    after
        0 -> Function()
    end.

inner_loop_helper(Waiter, State = {Id, CurrentCell, Reporter, StartingCell})
  when is_pid(Waiter), is_number(Id) ->
    receive
        {neighbors, Neighbors} ->
            priv_got_neighbors(Reporter, Neighbors, CurrentCell),
            inner_loop(Waiter, State);

        {move_to, Cell} ->
            reporter:report_move(Reporter, os:timestamp(), Id, cell:cell_id(Cell)),
            cell:ant_leaving(CurrentCell, self()),
            Waiter ! done,
            if
                StartingCell == undefined ->
                    loop(priv_statify(Id, Cell, Reporter, Cell));
                true ->
                    loop(priv_statify(Id, Cell, Reporter, StartingCell))
            end;

        move_failed ->
            Waiter ! done,
            loop(State);

        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, inner_loop_helper(Waiter, State)
    end.

loop_helper(State = {Id, CurrentCell, _}) ->
    receive
        {wakeup_and_move, Waiter} ->
            cell:tell_neighbors(CurrentCell, self()),
            inner_loop(Waiter, State);

        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, loop_helper(State)
    end.

inner_loop(Waiter, State) -> priority_wrapper(State, fun () -> inner_loop_helper(Waiter, State) end).
loop(State) -> priority_wrapper(State, fun () -> loop_helper(State) end).

%% public api
start(Id, Reporter) when is_number(Id) ->
    spawn(fun () ->
                  {A,B,C} = erlang:now(),
                  random:seed(A, B, C + Id),
                  loop({Id, undefined, Reporter})
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
