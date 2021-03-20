// dynamic programming.
// Separate problem into sub problems, compute subproblems
// and get the combination of the most optimal sub solutions to get
// optimal solution.
// If a subproblem is computed, we use memoization to remember the
// result to avoid duplicated complutation.

// there is no one dp problem, it's just a general methodology to
// solve problem.

// problem:
// Given a steel rod and price of different length. How to cut it
// to maxmize the price?

#include <array>

// TODO

// index is the length of a rod, the value at that index is the price.
//                           1  2  3  4   5  6   7   8
std::array<int, 8> rod_price{1, 5, 8, 9, 10, 17, 17, 20};
