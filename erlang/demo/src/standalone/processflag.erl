-module(processflag).
-export([start/0]).


start() ->
    register(demo, spawn(processflag, demo, [])).


demo() ->
    process_flag(trap_exit, true),
    demo1().

demo1() ->
    receive
        { 'EXIT', From, normal } ->
            io:format("Demo recevied normal exit from ~w~n", [From]),
            demo1();
        { 'EXIT', From, Reason } ->
            io:format("Demo recevied exit sginal ~w from ~w~n",
                      [Reason, From]),
            demo1();
        finished_demo ->
            io:format("Demo finished ~n", []);
        Other ->
            io:format("Demo process message ~w~n", [Other]),
            demo1()
    end.

demo_normal() ->
    link(whereis(demo)).

% demo_exit(normal) is the same as calling demo_normal.
% just a simple exit.
demo_exit(What) ->
    link(whereis(demo)),
    exit(What).

demo_message(What) ->
    demo ! What.

demo_error() ->
    link(whereis(demo)),
    1 = 2.
