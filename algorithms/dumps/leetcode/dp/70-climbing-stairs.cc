#include <cassert>
#include <iostream>
#include <vector>

using namespace std;

// dp[n] = dp[n - 1] + dp[n - 2]

class Solution {
public:
  int climbStairs(int n) {
    if (n <= 2)
      return n;
    std::vector<int> dp(n + 1, 1);
    for (int i = 2; i <= n; ++i)
      dp[i] = dp[i - 1] + dp[i - 2];
    return dp[n];
  }
};

int main(void) {

  Solution solution;
  {
    int n = 2;
    auto result = solution.climbStairs(n);
    std::cout << n << std::endl;
    assert(result == 2);
  }

  return 0;
}
