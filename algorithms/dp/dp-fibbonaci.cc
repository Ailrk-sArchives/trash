#include <iostream>

#include <chrono>
#include <functional>
#include <unordered_map>

// simple fibbonaci with dp.
// The idea of dp is to separate big problem into smaller prblems,
// solve optimal sub problems, combine solutions to solve the
// big problem.
//
// You can solve a dp problem bottom up or top down.
// Usually top-down implies a normal recursion solution, while
// bottom up will require you to construct the smallest problem then
// goes towards the final answer.
//
// By using dp we can work on looking for optimal subproblems, thus
// ignore suboptimal solutions. In this sense, dp can be think of
// as a way to shrink the possible solution space of a problem.

// dp fib is super simple, but it has several implication. it's
// uses buttom up approach. Since we know after we constructed
// the subsolution, solution for smaller problems can be ignored,
// so we can only use two variables to save the state.

// use dp to construct solution button up.
int fib_dp(int n) {
  int a = 0, b = 1, tmp;
  for (int i = 0; i < n; ++i) {
    tmp = b;
    b = a + b;
    a = tmp;
  }
  return b;
}

// use recursion, a top down approach.
// this solution has several problems:
//  1. It's not tail recursive. Because recursion will defer the
//     evaluation, stack space can be used up when input is large.
//  2. it's possible to recompute sub problems.
int fib_rec(int n) {
  if (n < 2) {
    return 1;
  }
  return fib_rec(n - 1) + fib_rec(n - 2);
}

// solve problem 2 for naive recursion.
// withotu recomputation, this version is as fast as iterative approach.
int fib_rec_mem(int n) {
  static std::unordered_map<int, int> cache{{0, 1}, {1, 1}};

  if (cache.find(n) != cache.end()) {
    return cache[n];
  }

  cache[n] = fib_rec_mem(n - 1) + fib_rec_mem(n - 2);
  return cache[n];
}

// of course we have the iterative method that everybody loves.
int fib_iter(int n) {
  static std::function<int(int, int, int)> fib_iter_ = [](int a, int b,
                                                          int n) -> int {
    if (n == 0) {
      return b;
    }
    return fib_iter_(b, a + b, n - 1);
  };

  return fib_iter_(0, 1, n);
}

// and we have cps transform
// it's trickier then most others.
int fib_cps(int n) {

  static auto ret = [](int a, int b) { return b; };
  static std::function<int(int a, int b, int n, std::function<int(int, int)>)>
      fib_cps_ = [](int a, int b, int n, auto k) -> int {
    if (n == 0) {
      return k(a, b);
    }

    return fib_cps_(a, b, n - 1,
                    [&](int a1, int b1) { return k(b1, a1 + b1); });
  };

  return fib_cps_(0, 1, n, ret);
}

#define N 40

#define START                                                                  \
  {                                                                            \
    auto start = std::chrono::system_clock::now();

#define END                                                                    \
  auto end = std::chrono::system_clock::now();                                 \
  auto elapsed =                                                               \
      std::chrono::duration_cast<std::chrono::milliseconds>(end - start);      \
  printf("time: %ld ms\n", static_cast<long int>(elapsed.count()));            \
  }

int main(void) {

  // 0ms
  START;
  printf("fib_dp: %d\n", fib_dp(N));
  END;

  // 839ms
  START;
  printf("fib_rec: %d\n", fib_rec(N));
  END;

  // 0ms
  START;
  printf("fib_rec_mem: %d\n", fib_rec_mem(N));
  END;

  // 0ms
  START;
  printf("fib_iter: %d\n", fib_iter(N));
  END;

  // 0ms
  START;
  printf("fib_cps: %d\n", fib_cps(N));
  END;

  return 0;
}

#undef N
