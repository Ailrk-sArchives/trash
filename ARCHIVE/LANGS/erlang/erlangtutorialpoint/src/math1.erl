
-module(math1).
-export([factorial/1]).
-export([double/1]).
-export([area/1]).
-export([convert/2]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

double(X) -> times(X, 2).
times(X, N) -> X * N.

% pattern matching with tuple.
% the first element can be treated as data constructor.
area({square, Side}) ->
    Side * Side;
area({rectangle, X, Y}) ->
    X * Y;
area({circle, Radius}) ->
    3.1415926 * Radius * Radius;
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

% data types
% There are primitive and compound data types.
% primitive: int, floats, atoms
% compound: tuples, list

convert({fahrenheit, Temp}, celsius) ->
    {celsius, 5 * (Temp - 32) / 9};
convert({reaumur, Temp}, celsius) ->
    {celsius, 10 * Temp / 8};
convert({celsius, Temp}, fahrenheit) ->
    {fahrenheit, 32 + Temp * 9 / 5};
convert({celsius, Temp}, reaumur) ->
    {reaumur, 8 * Temp / 10};
convert({X, _}, Y) ->
    {cannot, convert, X, to, Y}.
