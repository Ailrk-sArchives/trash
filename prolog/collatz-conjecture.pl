% let n be integer, define
% f(n) =  {
%     n / 2 if n = 0 (mod 2)
%     3n + 1 if n = 1 (mod 2)
% }
%
% Then consider hailstone sequence:
%    n -> f (n) -> f(f(n)) -> ...
%
% e.g. 1 -> 4 -> 2 -> 1 -> 4 -> 2 -> 1 -> ...
%      3 -> 10 -> 5 -> 16 -> 4 -> 2 -> 1 -> ...

% collatz conjecture:
%   integer 1 appears in hailstone sequence no matter from which positive
%   integer we start.

collatz_next(N0, N) :-
  N0 #= 2*N.
collatz_next(N0, N) :-
  N0 #= 2 * _ + 1,
  N #= 3 * N0 + 1.

?- collatz_next(5, N).

collatz_reaches(N, N).
collatz_reaches(N0, N) :-
  collatz_next(N0, N1),
  collatz_reaches(N1, N).
