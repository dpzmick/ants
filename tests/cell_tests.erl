-module(cell_tests).
-include_lib("eunit/include/eunit.hrl").

left_right_test_() ->
    {
     "test left/right neighbors",
     fun () ->
             C1 = cell:start(c1),
             C2 = cell:start(c2),
             cell:register_neighbor(C1, C2, e),
             cell:tell_neighbors(C1, self()),
             receive
                 {neighbors, Neighbors} ->
                     {_, Ret} = dict:find(e, Neighbors),
                     ?assert(Ret == C2)
             end,
             cell:tell_neighbors(C2, self()),
             receive
                 {neighbors, Neighbors1} ->
                     {_, Ret1} = dict:find(w, Neighbors1),
                     ?assert(Ret1 == C1)
             end
     end
    }.

up_down_test_() ->
    {
     "test left/right neighbors",
     fun () ->
             C1 = cell:start(c1),
             C2 = cell:start(c2),
             cell:register_neighbor(C1, C2, s),
             cell:tell_neighbors(C1, self()),
             receive
                 {neighbors, Neighbors} ->
                     {_, Ret} = dict:find(s, Neighbors),
                     ?assert(Ret == C2)
             end,
             cell:tell_neighbors(C2, self()),
             receive
                 {neighbors, Neighbors1} ->
                     {_, Ret1} = dict:find(n, Neighbors1),
                     ?assert(Ret1 == C1)
             end
     end
    }.

