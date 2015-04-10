-module(ant).
-export([start/1, wakeup_and_move/1, tell_neighbors/2, you_moved/2, ant_id/1]).

%% pick one of these randomly and try to move to it
priv_pick_neighbor(Neighbors) ->
    Cells = lists:map(
          fun({_,Cell}) -> Cell end,
          dict:to_list(Neighbors)),
    Index = random:uniform(length(Cells)),
    lists:nth(Index, Cells).

priv_got_neighbors(Neighbors, Id) ->
    Choice = priv_pick_neighbor(Neighbors),
    cell:move_ant_to(Choice, {Id, self()}).

loop({Id, CurrentCell}) ->
    receive
        wakeup_and_move ->
            case CurrentCell of
                undefined -> ok;
                _ -> cell:tell_neighbors(CurrentCell, {Id, self()}),
                     loop({Id, CurrentCell})
            end;

        {neighbors, Neighbors} ->
            priv_got_neighbors(Neighbors, Id),
            loop({Id, CurrentCell});

        {move_to, Cell} ->
            loop({Id, Cell})
    end.

%% public api
start(Id) ->
    {Id, spawn(fun () -> loop({Id, undefined}) end)}.

wakeup_and_move({_, Ant}) ->
    Ant ! wakeup_and_move.

tell_neighbors({_,Ant}, Neighbors) ->
    Ant ! {neighbors, Neighbors}.

you_moved({_, Ant}, ToCell) ->
    Ant ! {move_to, ToCell}.

ant_id({Id, _}) -> Id.
