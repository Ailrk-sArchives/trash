#include <cstring>
#include <algorithm>
#include <iostream>

// given an array of non duplicate integers, find the number of pairs
// that sum up to 0.

// it's an example of not obvious enumeration.
int num_sum_to_0(int *a, size_t n, int max) {
  int res = 0;
  bool met[max * 2];
  std::memset(met, 0, sizeof(met));

  for (int i = 0; i < n; ++i) {
    if (met[max - a[i]])
      res++;
    met[max + a[i]] = 1;
  }
  return res;
}

#define SIZE(xs) (sizeof(xs) / sizeof(xs[0]))

int main(void) {

  int a1[] = {1, 2, 5, -2, 3, -3, 9, 2};
  int a2[] = {-1, 2, -392, -2, 392, -4, 87, 1, 123, 3};

  int v1 = num_sum_to_0(a1, SIZE(a1), *std::max_element(a1, a1 + SIZE(a1)));
  std::cout << v1 << std::endl;

  int v2 = num_sum_to_0(a2, SIZE(a2), *std::max_element(a2, a2 + SIZE(a2)));
  std::cout << v2 << std::endl;

  return 0;
}
