-module(exn).
-export([start/0, generate_exception/1, handler1/1, handler2/1, process/0]).

% handling exceptions.
start() ->
    register(p1, spawn(exn, process, [])),
    register(p2, spawn(exn, process, [])),
    register(p3, spawn(exn, process, [])),
    p1 ! {stop, hello},
    p2 ! {stop, return},
    p3 ! {stop, exception},
    { [ handler1 (I) || I <- [0, 1, 2, 3, 4]],
      [ handler2 (I) || I <- [0, 1, 2, 3, 4]] }.

generate_exception(0) -> a;
generate_exception(1) -> throw(a);   % exception
generate_exception(2) -> exit(a);    % normal termination
generate_exception(3) -> {'EXIT', a};
generate_exception(4) -> erlang:error(a).

%
% try of catch
%
handler1(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X -> {N, caught, exited, X};
        error:X -> {X, caught, error, X}
    end.

%
% case catch
%

handler2(N) ->
    case catch generate_exception(N) of
        % throw, exit, and { 'EXIT', What } do the same thing.
        % erlang:error also result an { 'EXIT', What } but report the backtrace.
        {'EXIT', What} ->
            { N, caught, What };
        Other ->
            { N, Other }
    end.

%
% process termination
%

process() ->
    receive
        {stop, Method} ->
            case Method of
                return ->
                    true;
                _Other ->
                    exit(normal)
            end;
        _Other ->
            process()
    end.
