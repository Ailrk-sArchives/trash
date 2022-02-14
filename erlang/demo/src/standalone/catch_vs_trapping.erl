-module(catch_vs_trapping.)
-export([test/0, start1/0, start2/0 p/1]).


% will terminate
test1() ->
    spawn_link(catch_vs_trapping, p, [1]),
    receive
        X -> X
    end.

% will propage {'Exit', Pid, badmatch}
test2() ->
    process_flag(trap_exit, true),
    spawn_link(catch_vs_trapping, p, [1]),
    receive
        X -> X
    end.

% p(1) triggers { 'EXIT', Pid, badmatch}.
p(N) ->
    N = 2.
