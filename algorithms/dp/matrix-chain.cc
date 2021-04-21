#include <iostream>
// Notice matrix multiplications are associative. So the order of evaluation doesn't
// affect the result.
// But it does affect the cost of computation though.
//
// Imagine we want to chain multiplication of 2x3 matrix A1, 3x4 matrix A2, and 4x2 matrix A3.
// We have two possible orders:
//  case 1: (A1 x A2) x A3,
//  case 2: A1 x (A2 x A3)
// Notice the number of computation required for multiplication between matrix axb and
// matrix bxc is a . b . c
//  for case 1, we have 2.3.4 + 2.4.2 = 40 computations.
//  for case 2, we have (3.4.2) + 2.3.2 = 36 computations,
// so case 2 is more efficient.
//
// We want to generalize this process for arbitrary long chain of matrix multiplication
//
// We can solve the problem with dp.

// How do we know how many possible evaluation orders are there?
// little tool: catlan number
//            1    (2n)     (2n)!
//    Cₙ = ------- (  )  = -------   for n >= 0.
//          n + 1  ( n)    (n+1)!n!


