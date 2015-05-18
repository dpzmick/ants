-module(sim_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% Assumes run from the root (test script)

read_a_test_() ->
    {
     "read test_data/a.conf",
     fun () ->
             Conf = sim_config:from_file("tests/test_data/a.conf"),
             Xmax = sim_config:xmax(Conf),
             Ymax = sim_config:ymax(Conf),
             ?assert(Xmax == 10),
             ?assert(Ymax == 10),
             ?assert(sim_config:numants(Conf) == 10),
             ?assert(sim_config:runtime(Conf) == 30),
             Coords = [{X,Y} || X <- lists:seq(1,Xmax), Y <- lists:seq(1, Ymax)],
             lists:map(
               fun ({X,Y}) ->
                       if
                           X == Y -> ?assertEqual(sim_config:cell_weight(Conf,X,Y), 10.0);
                           true -> ?assertEqual(sim_config:cell_weight(Conf,X,Y), 1.0)
                       end
               end,
               Coords)
     end
    }.

read_b_test_() ->
    {
     "read test_data/b.conf",
     fun () ->
             Conf = sim_config:from_file("tests/test_data/b.conf"),
             Xmax = sim_config:xmax(Conf),
             Ymax = sim_config:ymax(Conf),
             ?assert(Xmax == 10),
             ?assert(Ymax == 10),
             ?assert(sim_config:numants(Conf) == 10),
             ?assert(sim_config:runtime(Conf) == 30),
             Coords1 = [{X,Y} || X <- lists:seq(1,5), Y <- lists:seq(1, 5)],
             lists:map(
               fun ({X,Y}) ->
                       if
                           X == Y ->
                               ?assertEqual(sim_config:cell_has_food(Conf,X,Y), true),
                               ?assertEqual(sim_config:cell_weight(Conf,X,Y), 1.0);

                           true ->
                               ?assertEqual(sim_config:cell_has_food(Conf,X,Y), false),
                               ?assertEqual(sim_config:cell_weight(Conf,X,Y), 1.0)
                       end
               end,
               Coords1),
             Coords2 = [{X,Y} || X <- lists:seq(6,10), Y <- lists:seq(6, 10)],
             lists:map(
               fun ({X,Y}) ->
                       if
                           X == Y ->
                               ?assertEqual(sim_config:cell_has_food(Conf,X,Y), false),
                               ?assertEqual(sim_config:cell_weight(Conf,X,Y), 10.0);

                           true ->
                               ?assertEqual(sim_config:cell_has_food(Conf,X,Y), false),
                               ?assertEqual(sim_config:cell_weight(Conf,X,Y), 1.0)
                       end
               end,
               Coords2)
     end
    }.
