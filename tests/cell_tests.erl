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
             end,
             cell:stop(C1),
             cell:stop(C2)
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
             end,
             cell:stop(C1),
             cell:stop(C2)
     end
    }.

ant_move_success_test_() ->
    {
     "ant move is successful",
     fun () ->
             C1 = cell:start(c1),
             cell:move_ant_to(C1, self()),
             receive
                 {move_to, ToCell} -> ?assert(C1 == ToCell)
             end,
             cell:stop(C1)
     end
    }.

ant_move_fail_test_() ->
    {
     "ant move failure",
     fun () ->
             C1 = cell:start(c1),
             A = ant:start(1, undefined),
             cell:move_ant_to(C1, A),
             cell:move_ant_to(C1, self()),
             receive
                 move_failed -> ?assert(0 == 0)
             end,
             cell:stop(C1),
             ant:stop(A)
     end
    }.

cell_get_id_test_() ->
    {
     "test getting cell's id",
     fun () ->
             C1 = cell:start(c1),
             ID = cell:cell_id(C1),
             ?assert(ID == c1)
     end
    }.
