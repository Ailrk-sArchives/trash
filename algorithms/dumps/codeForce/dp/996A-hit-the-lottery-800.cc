// https://codeforces.com/problemset/problem/996/A

#include <inttypes.h>
#include <iostream>

#define TEST

// bills = 1 + min(f(n - 1), f(n - 5), f(f - 10), f(n - 20), f(n - 100))

#define MIN(a, b) (a > b ? b : a)

int get_min(int n) {
  int f[1024];
  int c = 0;
  f[0] = 0;

  for (int i = 1; i <= n; ++i) {
    c = INT32_MAX;
    if (i >= 1) {
      c = MIN(c, f[i - 1] + 1);
    }
    if (i >= 5) {
      c = MIN(c, f[i - 5] + 1);
    }
    if (i >= 10) {
      c = MIN(c, f[i - 10] + 1);
    }
    if (i >= 20) {
      c = MIN(c, f[i - 20] + 1);
    }
    if (i >= 100) {
      c = MIN(c, f[i - 100] + 1);
    }

    f[i] = c;
  }

  return c;
}

int main(void) {
  std::ios_base::sync_with_stdio(false);

  int n;
  std::cin >> n;
  std::cout << get_min(n) << std::endl;

  return 0;
}
