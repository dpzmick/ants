-module(ant).
-export([start/2, wakeup_and_move/1, tell_neighbors/2, you_moved/2, failed_move/1, ant_id/1, stop/1]).

%% pick one of these randomly and try to move to it
priv_pick_neighbor(Neighbors) ->
    Cells = lists:map(fun({_,Cell}) -> Cell end, dict:to_list(Neighbors)),
    Index = random:uniform(length(Cells)),
    lists:nth(Index, Cells).

priv_got_neighbors(Neighbors) ->
    Choice = priv_pick_neighbor(Neighbors),
    cell:move_ant_to(Choice, self()).

priv_statify(Id, CurrentCell, Reporter) -> {Id, CurrentCell, Reporter}.

outer_loop(State) ->
    receive
        {stop, ToTell} ->
            ToTell ! stopped
    after
        0 -> loop(State)
    end.

inner_loop(Waiter, State = {Id, CurrentCell, Reporter}) ->
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

loop(State = {_, CurrentCell, _}) ->
    receive
        {wakeup_and_move, Waiter} ->
            cell:tell_neighbors(CurrentCell, self()),
            inner_loop(Waiter, State);

        {stop, ToTell} ->
            ToTell ! stopped
    end.

%% public api
start(Id, Reporter) ->
    spawn(fun () ->
                  {A,B,C} = erlang:now(),
                  random:seed(A, B, C + Id),
                  outer_loop({Id, undefined, Reporter})
          end).

%% blocks
wakeup_and_move(Ant) ->
    Ant ! {wakeup_and_move, self()},
    receive
        done -> ok
    end.

tell_neighbors(Ant, Neighbors) ->
    Ant ! {neighbors, Neighbors}.

you_moved(Ant, ToCell) ->
    Ant ! {move_to, ToCell}.

failed_move(Ant) ->
    Ant ! move_failed.

ant_id(Ant) ->
    Ant ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.

stop(undefined) -> ok;
stop(Ant) ->
    Ant ! {stop, self()},
    receive
        stopped -> ok
    end.
