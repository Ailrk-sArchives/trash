#include <iostream>

// euclidean traveling salesman problem.

// Traveling salesman problem is NP hard, we are looking for a constraint version of the
// problem: finding the shortest closed bitonic tour.
//
// Bitonic means after the graph is formed. any vertical line cuts the graph at most twice.


// The problem
// The problem of finding the shortest closed bitonic tour
// can be though as finding the shortest open bitonic tour: the tour that
// doesn't connect the last vertex with the starting vertex. And finding the shortest
// open bitonic tour itself can be solved by finding the shortest path with one node
// less, this recursively go down until hit the base case: only one edge left.
// PS: if there are only two points, the shortest open bitonic tour is just the line
//     connects them.

// Given n points
// We first label each points with an index from 0 to n-1. We can define the shortest
// path function C, so for two given points i, j, we have C(i, j) as the shortest
// open bitonic tour.

// Formulate the problem:
// IF we want to find the shortest closed bitonic tour for 5 points, with the funcitoon
// C defined above, the shortest tour is written as C(0, 5).
//
// we can list optimal solutions for all subproblems:
//
// C(0, 1)
// C(0, 2) C(1, 2)
// C(0, 3) C(1, 3) C(2, 3)
// C(0, 4) C(1, 4) C(2, 4) C(3, 4)
//
// Notice  C(0, 5) is just C(x, 4) + edge from 3 to 4. We formulate this relation as:
//  C(0, 4) = min(C(0, 4), C(1, 4), C(2, 4)) + (edge 4, 5)
//
// More generally, for a graph with n points, we have
//
//  C(0, n-1) = min(C(i, n-2)) + (edge of n-1 and n-2) forall 0 <= i <= n-1
//  C(0, 1) = a constant (just the line between 0 and 1)

// Notice overlapping subproblems. e.g For the example above, C(0, 2) needs to be solved
// four times.
// To make an efficient implementation, we can use memoization techniques to cache C(0, n) in
// a table. Whenever we're trying to solve C, search in the cache first.


int bitonic_shortest_tour() {
}


int main(void) {

  return 0;
}

