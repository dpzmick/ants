-module(ant).
-export([start/1, wakeup_and_move/1, tell_neighbors/2, you_moved/2, ant_id/1]).

%% pick one of these randomly and try to move to it
priv_pick_neighbor(Neighbors) ->
    Cells = lists:map(fun({_,Cell}) -> Cell end, dict:to_list(Neighbors)),
    Index = random:uniform(length(Cells)),
    lists:nth(Index, Cells).

priv_got_neighbors(Neighbors) ->
    Choice = priv_pick_neighbor(Neighbors),
    cell:move_ant_to(Choice, self()).

priv_statify(Id, CurrentCell) -> {Id, CurrentCell}.

loop(State = {Id, CurrentCell}) ->
    receive
        wakeup_and_move ->
            if
                CurrentCell =:= undefined -> ok;
                true -> cell:tell_neighbors(CurrentCell, self())
            end,
            loop(State);

        {neighbors, Neighbors} ->
            priv_got_neighbors(Neighbors),
            loop(State);

        {move_to, Cell} ->
            loop(priv_statify(Id, Cell));

        {tell_id, To} -> To ! {told_id, Id}, loop(State)
    end.

%% public api
start(Id) ->
    spawn(fun () -> loop({Id, undefined}) end).

wakeup_and_move(Ant) ->
    Ant ! wakeup_and_move.

tell_neighbors(Ant, Neighbors) ->
    io:format("ant: ~p neigh: ~p~n", [Ant, Neighbors]),
    Ant ! {neighbors, Neighbors}.

you_moved(Ant, ToCell) ->
    Ant ! {move_to, ToCell}.

ant_id(Ant) ->
    Ant ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.
