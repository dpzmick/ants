-module(cell).
-export([start/1, register_neighbor/3, tell_neighbors/2, move_ant_to/2]).

opposite_direction(Direction) ->
    case Direction of
        n -> s;
        s -> n;
        e -> w;
        w -> e
    end.

priv_register_neighbor(State, Cell, Direction) ->
    {D, C, Id} = State,
    loop({dict:store(Direction, Cell, D), C, Id}).

priv_move_ant_to(State, Ant) ->
    {_, C, Id} = State,
    io:format("Moving ant: ~p to ~p~n", [ant:ant_id(Ant), Id]),
    case C of
        undefined -> ant:you_moved(Ant, self());
        _ -> ok
    end,
    loop(State).

loop(State) ->
    receive
        {Cell, register_neighbor, Direction, FollowupDesired} ->
            if
                FollowupDesired ->
                    Cell ! {self(), register_neighbor, opposite_direction(Direction), false};
                true -> ok
            end,
            priv_register_neighbor(State, Cell, Direction);

        {Ant, who_are_your_neighbors} ->
            {D,_,_} = State,
            ant:tell_neighbors(Ant, D),
            loop(State);

        {Ant, move_me_to_you} -> priv_move_ant_to(State, Ant)
    end.

%% public api
start(Id) ->
    spawn(fun () -> loop({dict:new(), undefined, Id}) end).

register_neighbor(Cell, Neighbor, Direction) ->
    Cell ! {Neighbor, register_neighbor, Direction, true}.

tell_neighbors(Cell, ToWho) ->
    Cell ! {ToWho, who_are_your_neighbors}.

move_ant_to(Cell, Ant) ->
    Cell ! {Ant, move_me_to_you}.
