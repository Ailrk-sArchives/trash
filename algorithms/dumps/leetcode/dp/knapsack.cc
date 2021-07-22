#include <iostream>
#include <vector>

// given n items, each item has value v and weight w.
// given a knapsack with capacity W,
// Ask: what items should we take to maximize the value in the knapsack.
//
// Two variations:
//  1. complete knapsack: one can cut an item by fraction
//  . 0 1 knapsack: items are atomic, you either take an item or not.
//
// State transition matrix.
//
// dp[i][j]: maximum value when ith item is not greter then j.
//          (total remaining knapsack capacity)
// dp[i][j] | j=1  j=2  j=3  j=4  j=5
// ---------+--------------------------
//   i=0    |
//   i=1    |
//   i=2    |
//   i=3    |
//   i=4    |

int knapsack01_naive(std::vector<int> weights, std::vector<int> values, int n,
                     int w) {}

int knapsack01_space_compressed(std::vector<int> weights,
                                std::vector<int> values, int n, int w) {}

int main(void) { return 0; }
