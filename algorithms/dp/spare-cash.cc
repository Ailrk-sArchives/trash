#include <functional>
#include <iostream>
#include <limits>
#include <vector>

// Problem:
// We have a country has weird coin values like 1, 5, 11.
// Given a price of some merchants be n, find a combination
// that gives n with 1, 5, 11 that requires the least amount of
// coins.

// Attempt:
// 1. We can just brute force, but the time complexity will be
//    very bad.
// 2. We can try to use a greedy algorithm. Imagine if we have coins
//    with value 1, 5, 10, and each time we choose the largest
//    possible coin until we reach the price.
//    This doesn't work for 1, 5, 11 though. Imagine to get 15,
//    if we choose 11 first, we have  11 + 1 + 1 + 1, 4 coins,
//    but if we choose a smaller value, 5, then it will be
//    5 + 5 + 5, 3 coins.
//    The reason a greedy algorithm doesn't work here is because
//    it's lack of a overview of the problem.

// dp idea:
//   define cost function f(n), n is the input
//   the goal is to minimize f(15).
//   now, at n = 15, we have 3 choices: 1, 5, 11
//   corresponds to subproblem with size 14, 10, 4 respectively.
//   So now we need to find minimize the cost of these input, namely
//
//      min(f(14), f(10), f(4)).
//
//   thus the cost of f(15):
//
//      f(15) = min(f(14), f(10), f(4)) + 1
//
//   do this recursively until the procedure finish.
//   let's see

#define min(a, b) (a > b ? b : a)
#define N 15

// a very common scheme on these competetive programming problem is to use array
// as a map from integer to some value.
// It's very fast and simple to use. it will almost never be the same case in a
// real world problem though.

void spare_cash1() {

  int f[256];
  int cost = 0;
  f[0] = 0;

  for (int i = 1; i <= N; ++i) {
    cost = INT32_MAX;

    // min(f(n - 1), f(n - 5), f(n - 11)) + 1
    if (i - 1 >= 0)
      cost = min(cost, f[i - 1] + 1);
    if (i - 5 >= 0)
      cost = min(cost, f[i - 5] + 1);
    if (i - 11 >= 0)
      cost = min(cost, f[i - 11] + 1);
    f[i] = cost;

    printf("f(%d) = %d\n", i, f[i]);
  }
}

// recursive solution.
void spare_cash2() {

  static std::vector<int> cache{};
  cache.resize(15);
  std::fill(cache.begin(), cache.end(), -1);
  cache[0] = 0;

  std::function<int(int)> rec = [&](int n) -> int {
    int cached = cache[n - 1];
    if (cached != -1) {
      printf("f(%d) = %d\n", n, cached);
      return cached;
    }

    int cost = INT8_MAX;
    if (n - 1 >= 0)
      cost = min(cost, rec(n - 2) + 1);
    if (n - 5 >= 0)
      cost = min(cost, rec(n - 5) + 1);
    if (n - 11 >= 0)
      cost = min(cost, rec(n - 11) + 1);

    printf("f(%d) = %d\n", n, cost);
    cache[n - 1] = cost;
    return cost;
  };

  rec(N);
}
// output:
// f(1) = 1
// f(2) = 2
// f(3) = 3
// f(4) = 4
// f(5) = 1
// f(6) = 2
// f(7) = 3
// f(8) = 4
// f(9) = 5
// f(10) = 2
// f(11) = 1
// f(12) = 2
// f(13) = 3
// f(14) = 4
// f(15) = 3

// we construct best solution from 1 to 15.

// 1. Why it's better than brute force? Because it only check cases of
//    f(n - 1) f(n - 5) f(n - 11). A brute force solution will require to
//    check all combinations.
// 2. Why it's better then greedy algorithm?
//    The final solution is made from sub optimal solutions. At f(n) it knows
//    min(f(n - 1) f(n - 5) f(n - 11)) will give it the best choice.
//    Greedy algorithm knows nothing about how good a choice is. It just follows
//    one rule and one rule only.

// core idea is to formulate the problem recursively.
// Although normally they can be solved iteratively.

int main(void) {
  spare_cash1();
  printf("\n");
  spare_cash2();
  return 0;
}
