-module(foo_behaviour_impl).
-behaviour(foo_behaviour).

-export([foo/1]).

% implement the interface here.

foo(1) -> ok;
foo(_) -> not_one.
