-module(ant).
-export([start/2, wakeup_and_move/1, tell_neighbors/2, you_moved/2, failed_move/1, ant_id/1, stop/1]).

%% pick one of these randomly (using their weight) and try to move to it
priv_pick_neighbor(Neighbors) ->
    Cells = lists:map(fun({_,Cell}) -> Cell end, dict:to_list(Neighbors)),
    Weights = lists:map(fun(C) -> cell:cell_weight(C) end, Cells),
    Sum = lists:sum(Weights),
    Probabilities = lists:map(fun(W) -> W / Sum end, Weights),
    HackyWeightedList = lists:foldl(
                          fun({P,C}, Acc) -> [C || _ <- lists:seq(1, round(P*100))] ++ Acc end,
                          [], lists:zip(Probabilities, Cells)),
    Index = random:uniform(length(HackyWeightedList)),
    lists:nth(Index, HackyWeightedList).

priv_got_neighbors(Neighbors) ->
    Choice = priv_pick_neighbor(Neighbors),
    cell:move_ant_to(Choice, self()).

priv_statify(Id, CurrentCell, Reporter) -> {Id, CurrentCell, Reporter}.

outer_loop(State = {Id, _, _}) when is_number(Id) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, outer_loop(State)
    after
        0 -> loop(State)
    end.

inner_loop(Waiter, State = {Id, CurrentCell, Reporter}) when is_pid(Waiter), is_number(Id) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped;

        {neighbors, Neighbors} ->
            priv_got_neighbors(Neighbors),
            inner_loop(Waiter, State);

        {move_to, Cell} ->
            reporter:report_move(Reporter, os:timestamp(), Id, cell:cell_id(Cell)),
            cell:ant_leaving(CurrentCell, self()),
            Waiter ! done,
            outer_loop(priv_statify(Id, Cell, Reporter));

        move_failed ->
            Waiter ! done,
            outer_loop(State);

        {tell_id, To} -> To ! {told_id, Id}, outer_loop(State)
    end.

loop(State = {Id, CurrentCell, _}) when is_number(Id) ->
    receive
        {wakeup_and_move, Waiter} ->
            cell:tell_neighbors(CurrentCell, self()),
            inner_loop(Waiter, State);

        {stop, ToTell} ->
            ToTell ! stopped;

        {tell_id, To} -> To ! {told_id, Id}, outer_loop(State)
    end.

%% public api
start(Id, Reporter) when is_number(Id) ->
    spawn(fun () ->
                  {A,B,C} = erlang:now(),
                  random:seed(A, B, C + Id),
                  outer_loop({Id, undefined, Reporter})
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
