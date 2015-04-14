-module(cell).
-export([start/1, register_neighbor/3, tell_neighbors/2, move_ant_to/2, ant_leaving/2, cell_id/1]).

priv_statify(Dict, Occupant, Id) -> {Dict, Occupant, Id}.

opposite_direction(Direction) ->
    case Direction of
        n -> s;
        s -> n;
        e -> w;
        w -> e
    end.

priv_register_neighbor({Dict, Occupant, Id}, Cell, Direction) ->
    outer_loop(priv_statify(dict:store(Direction, Cell, Dict), Occupant, Id)).

priv_move_ant_to(State = {Dict, Occupant, Id}, Ant) ->
    case Occupant of
        undefined -> ant:you_moved(Ant, self()), loop(priv_statify(Dict, Ant, Id));
        _ -> outer_loop(State)
    end.

outer_loop(State = {_,_,Id}) ->
    receive
        stop -> io:format("Cell ~p stopping~n", [Id]), ok
    after
        0 -> loop(State)
    end.

loop(State = {Dict, _Occupant, Id}) ->
    receive
        {Cell, register_neighbor, Direction, true} ->
            Cell ! {self(), register_neighbor, opposite_direction(Direction), false},
            priv_register_neighbor(State, Cell, Direction);

        {Cell, register_neighbor, Direction, false} -> priv_register_neighbor(State, Cell, Direction);

        {Ant, who_are_your_neighbors} ->
            ant:tell_neighbors(Ant, Dict),
            outer_loop(State);

        {Ant, move_me_to_you} -> priv_move_ant_to(State, Ant);

        {_Ant, ive_left} -> outer_loop(priv_statify(Dict, undefined, Id));

        {tell_id, ToWho} -> ToWho ! {told_id, Id}, outer_loop(State)
    end.

%% public api
start(Id) ->
    spawn(fun () -> outer_loop({dict:new(), undefined, Id}) end).

register_neighbor(Cell, Neighbor, Direction) ->
    Cell ! {Neighbor, register_neighbor, Direction, true}.

tell_neighbors(Cell, ToWho) ->
    Cell ! {ToWho, who_are_your_neighbors}.

move_ant_to(Cell, Ant) ->
    Cell ! {Ant, move_me_to_you}.

ant_leaving(Cell, Ant) ->
    Cell ! {Ant, ive_left}.

cell_id(Cell) ->
    Cell ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.
