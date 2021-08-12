#include <iostream>

#include <cmath>
#include <vector>
using namespace std;

namespace S1 {
class Solution {
  // 248ms
public:
  vector<int> closestDivisors(int num) {
    int s = sqrt(num);
    while (s * s < num)
      ++s;
    int a = s, b = s;
    int p = a * b;
    while (p != num + 1 && p != num + 2) {
      if (p < num + 1) {
        a += 1;
      } else if (p > num + 2) {
        b -= 1;
      }
      p = a * b;
    }
    return {a, b};
  }
};
} // namespace S1

// NOTE:
// if both divisors moves but the target is fixed, we can traverse on one
// and mod it to get another.
namespace S2 {
class Solution {
  // 4ms
  vector<int> closestDivisors(int x) {
    for (int a = sqrt(x + 2); a > 0; --a) {
      if ((x + 1) % a == 0)
        return {a, (x + 1) / a};
      if ((x + 2) % a == 0)
        return {a, (x + 2) / a};
    }
    return {};
  };
};

}; // namespace S2

template <typename S> void test_sqrt(S &s) {
  for (int i = 0; i < 50; ++i) {
    std::cout << i << " " << sqrt(i) << std::endl;
  }
}

template <typename C> void prints(const C &c) {
  for (auto &n : c)
    std::cout << n << " ";
  std::cout << std::endl;
}

template <typename S> void test1(S &s) {
  std::vector<int> input{8, 123, 999};
  for (auto &n : input) {
    std::cout << "input: " << n << std::endl;
    prints(s.closestDivisors(n));
  }
}

int main(void) {
  {
    using namespace S1;
    Solution s;
    test1(s);
  }

  return 0;
}
