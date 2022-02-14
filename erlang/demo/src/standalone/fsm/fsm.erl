-module(fsm).
-export([start/0, s1/0, s2/0, s3/0, s4/0]).

start() ->
    spawn(fsm, s1, []).


s1() ->
    receive
        msg_a -> s2();
        msg_c -> s3()
    end.

s2() ->
    receive
        msg_h -> s4();
        msg_x -> s3()
    end.

s3() ->
    receive
        msg_b -> s1();
        msg_y -> s2();
    end.

s4() ->
    receive
        msg_i -> s3();
        msg_h -> s2();
    end.
