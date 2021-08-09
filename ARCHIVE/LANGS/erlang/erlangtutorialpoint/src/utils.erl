-module(utils).
-export([reverse/1, factorial/1, checkType/1, distance/2]).
-export([point/2, rect/2, circle/2, square/2, whatShape/1]).

% function with same name but different arities are
% completely different functions.
% Here we are using c++ style overloading.
% In haskell this will requires a helper.
reverse(L) -> reverse(L, []).
reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L) -> L.

% when is the guard.
factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

% contract
checkType(X) when is_atom(X) -> atom;
checkType(X) when is_list(X) -> lists;
checkType(X) when is_integer(X) -> integer;
checkType(X) when is_float(X) -> float;
checkType(X) when is_tuple(X) -> tuple;
checkType(X) when is_tuple(X) -> tuple;
checkType(X) when is_reference(X) -> reference;
checkType(X) when is_binary(X) -> binary.


distance({point, X1, Y1}, {point, X2, Y2}) ->
    math:sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)).

rect({size, W, H}, {point, X, Y}) ->
    P1 = point(X, Y),
    P2 = point(X+W, Y),
    P3 = point(X, Y+H),
    P4 = point(X+W, Y+H),
    {rect, P1, P2, P3, P4}.

% use function to create datatype.
point(X, Y) when is_float(X) and is_float(Y) -> {point, X, Y}.

circle(Radius, {point, X, Y}) ->
    {circle, Radius, {point, X, Y}}.

square(L, Point) -> rect({size, L, L}, Point).

whatShape(X) ->
    case X of
        {circle, Radius, _} -> if
                                   Radius > 100 -> bigCircle;
                                   true -> smallCircle
                               end;
        {square, _, _, _, _} -> square;
        {rect, _, _, _, _} -> rect
    end.
