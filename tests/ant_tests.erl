-module(ant_tests).
-include_lib("eunit/include/eunit.hrl").

tell_id_test_() ->
    {
     "tell me your id",
     fun () ->
             A = ant:start(1, undefined),
             ID = ant:ant_id(A),
             ?assert(ID == 1),
             ant:stop(A)
     end
    }.
