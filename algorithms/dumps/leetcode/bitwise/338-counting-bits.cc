#include <cassert>
#include <iostream>
#include <vector>
using namespace std;
template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

namespace just_count {
// simply count
class Solution {
public:
  vector<int> countBits(int n) {
    std::vector<int> result;
    for (int i = 0; i <= n; ++i) {
      result.push_back(count(i));
    }
    return result;
  }

  int count(int n) {
    int counter = 0;
    while (n) {
      if (n & 1) {
        ++counter;
      }
      n >>= 1;
    }
    return counter;
  }
};
} // namespace just_count

namespace dp {
// use dp to solve it.
// n is a funciton of i that returns number of 1s in the binary form of i.
//   if i == 0, n(i) = 0
//   if i is odd, n(i) = n(i - 1) + 1
//   if i is even, n(i) = n(i >> 1)
// with base case 0, we can induce the solution for all i.
class Solution {
public:
  vector<int> countBits(int n) {
    std::vector<int> dp(n + 1, 0);
    for (int i = 1; i <= n; ++i) {
      dp[i] = i & 1 ? dp[i - 1] + 1 : dp[i >> 1];
    }
    return dp;
  }
};

} // namespace dp

int test_cases(auto solution) {

  {
    int n = 2;
    auto result = solution.countBits(n);

    print_seq(result);
    assert(result == (std::vector<int>{0, 1, 1}));
  }

  {
    int n = 5;
    auto result = solution.countBits(n);

    print_seq(result);
    assert(result == (std::vector<int>{0, 1, 1, 2, 1, 2}));
  }

  return 0;
}

int main(void) {
  {
    using namespace just_count;
    Solution solution;
    test_cases(solution);
  }

  {
    using namespace dp;
    Solution solution;
    test_cases(solution);
  }
  return 0;
}
