-module(foo_behaviour).

% define a behaviour here, it's essentially an interface.
-callback foo(integer()) -> atom().
