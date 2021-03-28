#include <array>
#include <cinttypes>
#include <functional>
#include <iostream>
#include <vector>

// array used as a map           x  1  2  3  4  5   6   7   8   9   10
std::vector<int32_t> rod_prices{-1, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30};

// cost of r[n]: r[n] = max(p + r[n-1])

#define MAX(a, b) (a > b ? a : b)

// naive recursion
int32_t cut_rod(const std::vector<int32_t> &prices, int32_t n) {
  if (n == 0) {
    return 0;
  }

  int32_t q = INT32_MIN;
  for (int32_t i = 1; i <= n; ++i) {
    q = MAX(q, prices[i] + cut_rod(prices, n - i));
  }

  return q;
}

// memoization version
int32_t cut_rod_mem(const std::vector<int32_t> &prices, int32_t n) {
  static std::vector<int32_t> cache(prices.size(), -1);
  if (cache[n] != -1)
    return cache[n];

  if (n == 0) {
    return 0;
  }

  int32_t q = INT32_MIN;
  for (int32_t i = 1; i <= n; ++i) {
    q = MAX(q, prices[i] + cut_rod(prices, n - i));
  }

  if (cache[n] == -1) {
    cache[n] = q;
  }
  return q;
}

int32_t cut_rod_bottomup(const std::vector<int32_t> &prices, int32_t n) {
  static std::vector<int32_t> result(prices.size(), 0);
  result[0] = 0;
  for (int32_t j = 1; j <= n; ++j) {
    int32_t q = INT32_MIN;
    for (int32_t i = 1; i <= j; ++i) {
      q = MAX(q, prices[i] + result[j - i]);
    }
    result[j] = q;
  }
  return result[n];
}

#define PRINT(n, x) std::cout << n << " " #x ": " << x << std::endl;
#define PICK(i, from, to, pred, stmt)                                          \
  for (int i = from; i <= to; ++i) {                                           \
    if (pred) {                                                                \
      PRINT(i, stmt);                                                          \
    }                                                                          \
  }

int main(void) {
  PICK(i, 1, 10, i == 5 || i == 8 || i == 9, cut_rod(rod_prices, i));
  PICK(i, 1, 10, i == 5 || i == 8 || i == 9, cut_rod_mem(rod_prices, i));
  PICK(i, 1, 10, i == 5 || i == 8 || i == 9, cut_rod_bottomup(rod_prices, i));

  return 0;
}

// OUTPUT:
// 5 cut_rod(rod_prices, i): 13
// 8 cut_rod(rod_prices, i): 22
// 9 cut_rod(rod_prices, i): 25
// 5 cut_rod_mem(rod_prices, i): 13
// 8 cut_rod_mem(rod_prices, i): 22
// 9 cut_rod_mem(rod_prices, i): 25
// 5 cut_rod_bottomup(rod_prices, i): 13
// 8 cut_rod_bottomup(rod_prices,i): 22
// 9 cut_rod_bottomup(rod_prices, i): 25
