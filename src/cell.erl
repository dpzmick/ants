-module(cell).
-export([start/1, start/3, register_neighbor/3, tell_neighbors/2, move_ant_to/2,
         ant_leaving/2, cell_id/1, eq/2, at_edge/1, has_food/1, cell_weight/1,
         set_weight/2, distance/2, stop/1]).

priv_statify(Dict, Occupant, Weight, Id, HasFood) -> {Dict, Occupant, Weight, Id, HasFood}.

opposite_direction(Direction) ->
    case Direction of
        n -> s;
        s -> n;
        e -> w;
        w -> e
    end.

priv_register_neighbor({Dict, Occupant, Weight, Id, HasFood}, Cell, Direction) ->
    outer_loop(priv_statify(dict:store(Direction, Cell, Dict), Occupant, Weight, Id, HasFood)).

priv_move_ant_to(State = {Dict, Occupant, Weight, Id, HasFood}, Ant) ->
    case Occupant of
        undefined -> ant:you_moved(Ant, self()), outer_loop(priv_statify(Dict, Ant, Weight, Id, HasFood));
        _ -> ant:failed_move(Ant), outer_loop(State)
    end.

outer_loop(State) ->
    receive
        {stop, ToWho} ->
            ToWho ! stopped
    after
        0 -> loop(State)
    end.

loop(State = {Dict, Occupant, Weight, Id, HasFood}) ->
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

        {_Ant, ive_left} -> outer_loop(priv_statify(Dict, undefined, Weight, Id, HasFood));

        {tell_id, ToWho} -> ToWho ! {told_id, Id}, outer_loop(State);

        {tell_weight, ToWho} -> ToWho ! {told_weight, Weight}, outer_loop(State);

        {has_food, ToWho} -> ToWho ! {has_food, HasFood}, outer_loop(State);

        {at_edge, ToWho} ->
            case dict:find(n, Dict) of
                {ok, _} ->
                    case dict:find(s, Dict) of
                        {ok, _} ->
                            ToWho ! {at_edge, false}, outer_loop(State);
                        error ->
                            ToWho ! {at_edge, true}, outer_loop(State)
                    end;
                error ->
                    ToWho ! {at_edge, true}, outer_loop(State)
            end;

        {update_weight, NewWeight} when is_number(NewWeight) ->
            outer_loop(priv_statify(Dict, Occupant, NewWeight, Id, HasFood))
    end.

%% public api
start(Id) -> start(Id, 1, false).

start(Id, Weight, HasFood) when is_number(Weight) ->
    spawn(fun () -> outer_loop({dict:new(), undefined, Weight, Id, HasFood}) end).

register_neighbor(Cell, Neighbor, Direction) when is_pid(Cell), is_pid(Neighbor) ->
    Cell ! {Neighbor, register_neighbor, Direction, true}.

tell_neighbors(undefined, _) -> ok;
tell_neighbors(Cell, ToWho) when is_pid(Cell), is_pid(ToWho) ->
    Cell ! {ToWho, who_are_your_neighbors}.

move_ant_to(Cell, Ant) when is_pid(Cell), is_pid(Ant) ->
    Cell ! {Ant, move_me_to_you}.

ant_leaving(undefined, _) -> ok;
ant_leaving(Cell, Ant) when is_pid(Cell), is_pid(Ant) ->
    Cell ! {Ant, ive_left}.

cell_id(undefined) -> undefined;
cell_id(Cell) when is_pid(Cell) ->
    Cell ! {tell_id, self()},
    receive
        {told_id, Id} -> Id
    end.

eq(Cell1, Cell2) ->
    Id1 = cell_id(Cell1),
    Id2 = cell_id(Cell2),
    Id1 == Id2.

at_edge(Cell) ->
    Cell ! {at_edge, self()},
    receive
        {at_edge, Value} -> Value
    end.

has_food(undefined) -> false;
has_food(Cell) when is_pid(Cell) ->
    Cell ! {has_food, self()},
    receive
        {has_food, Bool} -> Bool
    end.

cell_weight(Cell) when is_pid(Cell) ->
    Cell ! {tell_weight, self()},
    receive
        {told_weight, Weight} -> Weight
    end.

set_weight(Cell, NewWeight) when is_number(NewWeight) ->
    Cell ! {update_weight, NewWeight}.

distance(Cell1, Cell2) ->
    {X1, Y1} = cell_id(Cell1),
    {X2, Y2} = cell_id(Cell2),
    T1 = math:pow(X1 - X2, 2),
    T2 = math:pow(Y1 - Y2, 2),
    math:sqrt(T1 + T2).

stop(undefined) -> ok;
stop(Cell) when is_pid(Cell) ->
    Cell ! {stop, self()},
    receive
        stopped -> ok
    end.
