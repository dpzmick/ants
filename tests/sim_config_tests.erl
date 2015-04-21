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
