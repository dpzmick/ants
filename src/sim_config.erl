-module(sim_config).
-export([from_file/1, xmax/1, ymax/1, numants/1, runtime/1, cell_weight/3]).

%% you'd better hope the file is formatted correctly

line_helper(Line, Expected) ->
    case string:tokens(string:strip(Line, both, $\n), ": ") of
        [Expected, Value] ->
            {V, _} = string:to_integer(Value), V;
        _ -> {error, malformed}
    end.

readline_helper(Fd, Expected) ->
    case file:read_line(Fd) of
        {ok, Line} -> line_helper(Line, Expected);
        eof -> {error, malformed};
        {error, Reason} -> {error, Reason}
    end.

handle_cell_line(Line) ->
    lists:map(fun(E) -> {V,_} = string:to_integer(E), V end,
              string:tokens(string:strip(Line, both, $\n), ",")).

iter_cell_config(Fd) -> iter_cell_config(Fd, dict:new()).

iter_cell_config(Fd, Dict) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            [X,Y,Weight] = handle_cell_line(Line),
            iter_cell_config(Fd, dict:store({X,Y}, Weight, Dict));
        eof -> Dict;
        {error, Reason} -> {error, Reason}
    end.

%% read the config file
reader_helper(Fd) ->
    Xmax          = readline_helper(Fd, "Xmax"),
    Ymax          = readline_helper(Fd, "Ymax"),
    NumAnts       = readline_helper(Fd, "NumAnts"),
    Runtime       = readline_helper(Fd, "Runtime"),
    DefaultWeight = readline_helper(Fd, "DefaultWeight"),
    %% read the empty line and the header
    file:read_line(Fd),
    file:read_line(Fd),
    BoundCells = iter_cell_config(Fd),
    file:close(Fd),
    {Xmax, Ymax, NumAnts, Runtime, DefaultWeight, BoundCells}.

from_file(Filename) ->
    case file:open(Filename, [read]) of
        {ok, Fd} -> reader_helper(Fd);
        {error, Reason} -> {error, Reason}
    end.

xmax({Xmax,_,_,_,_,_}) -> Xmax.
ymax({_,Ymax,_,_,_,_}) -> Ymax.
numants({_,_,NumAnts,_,_,_}) -> NumAnts.
runtime({_,_,_,Runtime,_,_}) -> Runtime.

cell_weight({_,_,_,_,DefaultWeight,Bound}, X, Y) ->
    case dict:find({X,Y}, Bound) of
        {ok, Weight} -> Weight;
        error -> DefaultWeight
    end.
