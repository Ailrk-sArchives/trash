#include <cassert>
#include <iostream>
#include <vector>

using namespace std;

// dp[n] = max(dp[i - 1], dp[i - 2] + nums[i - 1])
class Solution {
public:
  int rob(vector<int> &nums) {
    if (nums.empty())
      return 0;
    int n = nums.size();
    std::vector<int> dp(n + 1, 0);
    dp[1] = nums[0];
    for (int i = 2; i <= n; ++i)
      dp[i] = std::max(dp[i - 1], nums[i - 1] + dp[i - 2]);
    return dp[n];

  }
};

int main(void) {

  Solution solution;

  {
    std::vector<int> v{1, 2, 3, 1};
    auto result = solution.rob(v);
    std::cout << result << std::endl;
    assert(result == 4);
  }
  return 0;
}
