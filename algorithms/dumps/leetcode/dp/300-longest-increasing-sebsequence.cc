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

using namespace std;

// find the length of the longest strictly increasing subsequence.
namespace dp_sol {

// dp[n]: longest subsequence up to index n.
// set all dp[n] = 1;
// for dp[i] dp[j] where j < i, if dp[j] < dp[i], dp[i] = min(dp[i], dp[j] + 1)
// why I spent so long on this?
class Solution {
public:
  int lengthOfLIS(vector<int> &nums) {
    int n = nums.size();
    int max = 1;
    std::vector<int> dp(n, 1);

    for (int i = 1; i < n; ++i) {
      for (int j = i; j >= 0; --j) {
        if (nums[j] < nums[i]) {
          dp[i] = std::max(dp[i], dp[j] + 1);
        }
        max = std::max(max, dp[i]);
      }
    }

    return max;
  }
};
} // namespace dp_sol
namespace bin_sol {

// lower_bound to
class Solution {
public:
  int lengthOfLIS(vector<int> &nums) {
    std::vector<int> result;
    for (int i = 0; i < nums.size(); ++i) {
      auto it = std::lower_bound(result.begin(), result.end(), nums[i]);
      if (it != result.end())
        *it = nums[i];
      else
        result.push_back(nums[i]);
    }
    return result.size();
  }
};
} // namespace bin_sol

void test_case(auto solution) {
  std::vector<std::pair<std::vector<int>, int>> inputs{

      {{10, 9, 2, 5, 3, 7, 101, 18}, 4}, {{0, 1, 0, 3, 2, 3}, 4},
      {{7, 7, 7, 7, 7, 7, 7}, 1},        {{4, 10, 4, 3, 8, 9}, 3},
      {{1, 3, 6, 7, 9, 4, 10, 5, 6}, 6}, {{0}, 1}};

  for (auto &[nums, a] : inputs) {
    auto result = solution.lengthOfLIS(nums);
    std::cout << "result: " << result << ", expected: " << a << std::endl;
    assert(result == a);
  }
}

int main() {

  {
    using namespace dp_sol;
    Solution solution;
    test_case(solution);
  }

  {
    using namespace bin_sol;
    Solution solution;
    test_case(solution);
  }
}
