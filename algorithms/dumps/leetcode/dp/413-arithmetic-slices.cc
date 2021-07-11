#include <cassert>
#include <iostream>
#include <numeric>
#include <vector>

using namespace std;

class Solution {
public:
  int numberOfArithmeticSlices(vector<int> &nums) {
    if (nums.size() < 3)
      return 0;
    std::vector<int> dp(nums.size(), 0);
    for (int i = 2; i < nums.size(); ++i) {
      if (nums[i] - nums[i - 1] == nums[i - 1] - nums[i - 2])
        dp[i] = dp[i - 1] + 1;
    }
    return std::accumulate(dp.begin(), dp.end(), 0);
  }
};

int main(void) {
  Solution solution;

  {
    std::vector<int> v{1, 2, 3, 4};
    auto result = solution.numberOfArithmeticSlices(v);
    std::cout << result << std::endl;
    assert(result = 3);
  }

  {
    std::vector<int> v{1, 2, 3, 8, 9, 10};
    auto result = solution.numberOfArithmeticSlices(v);
    std::cout << result << std::endl;
    assert(result = 2);
  }

  return 0;
}
