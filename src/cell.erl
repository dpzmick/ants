-module(cell).
-export([start/1, start/2, register_neighbor/3, tell_neighbors/2, move_ant_to/2,
         ant_leaving/2, cell_id/1, cell_weight/1, stop/1]).

priv_statify(Dict, Occupant, Weight, Id) -> {Dict, Occupant, Weight, Id}.

opposite_direction(Direction) ->
    case Direction of
        n -> s;
        s -> n;
        e -> w;
        w -> e
    end.

priv_register_neighbor({Dict, Occupant, Weight, Id}, Cell, Direction) ->
    outer_loop(priv_statify(dict:store(Direction, Cell, Dict), Occupant, Weight, Id)).

priv_move_ant_to(State = {Dict, Occupant, Weight, Id}, Ant) ->
    case Occupant of
        undefined -> ant:you_moved(Ant, self()), outer_loop(priv_statify(Dict, Ant, Weight, Id));
        _ -> ant:failed_move(Ant), outer_loop(State)
    end.

outer_loop(State) ->
    receive
        {stop, ToWho} ->
            ToWho ! stopped
    after
        0 -> loop(State)
    end.

loop(State = {Dict, _Occupant, Weight, Id}) ->
    receive
        {stop, ToWho} ->
            ToWho ! stopped;

        {Cell, register_neighbor, Direction, true} ->
            Cell ! {self(), register_neighbor, opposite_direction(Direction), false},
            priv_register_neighbor(State, Cell, Direction);

        {Cell, register_neighbor, Direction, false} -> priv_register_neighbor(State, Cell, Direction);

        {Ant, who_are_your_neighbors} ->
            ant:tell_neighbors(Ant, Dict),
            outer_loop(State);

        {Ant, move_me_to_you} -> priv_move_ant_to(State, Ant);

        {_Ant, ive_left} -> outer_loop(priv_statify(Dict, undefined, Weight, Id));

        {tell_id, ToWho} -> ToWho ! {told_id, Id}, outer_loop(State);

        {tell_weight, ToWho} -> ToWho ! {told_weight, Weight}, outer_loop(State)
    end.

%% public api
start(Id) -> start(Id, 1).

start(Id, Weight) ->
    spawn(fun () -> outer_loop({dict:new(), undefined, Weight, Id}) end).

register_neighbor(Cell, Neighbor, Direction) ->
    Cell ! {Neighbor, register_neighbor, Direction, true}.

tell_neighbors(undefined, _) -> ok;
tell_neighbors(Cell, ToWho) ->
    Cell ! {ToWho, who_are_your_neighbors}.

move_ant_to(Cell, Ant) ->
    Cell ! {Ant, move_me_to_you}.

ant_leaving(undefined, _) -> ok;
ant_leaving(Cell, Ant) ->
    Cell ! {Ant, ive_left}.

cell_id(Cell) ->
    Cell ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.

cell_weight(Cell) ->
    Cell ! {tell_weight, self()},
    receive
        {told_weight, Weight} -> Weight
    end.

stop(undefined) -> ok;
stop(Cell) ->
    Cell ! {stop, self()},
    receive
        stopped -> ok
    end.
