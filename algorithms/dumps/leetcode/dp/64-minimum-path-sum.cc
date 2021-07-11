#include <cassert>
#include <iostream>
#include <vector>

// 2d dynamic programming
// dp[i][j] = min(dp[i][j - 1], dp[i - 1][j]) - nums[i][j]

using namespace std;

class Solution {
public:
  int minPathSum(vector<vector<int>> &grid) {
    const int m = grid.size(), n = grid[0].size();
    std::vector<std::vector<int>> dp(m, std::vector<int>(n, 0));

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        dp[i][j] = (i == 0 && j == 0) ? grid[i][j]
                   : (i == 0)         ? dp[i][j - 1] + grid[i][j]
                   : (j == 0)
                       ? dp[i - 1][j] + grid[i][j]
                       : std::min(dp[i - 1][j], dp[i][j - 1]) + grid[i][j];
      }
    }
    return dp[m - 1][n - 1];
  }
};

int main() {
  Solution solution;

  {
    std::vector<std::vector<int>> m{{1, 3, 1}, {1, 5, 1}, {4, 2, 1}};
    auto result = solution.minPathSum(m);
    std::cout << result << std::endl;
    assert(result == 7);
  }

  {
    std::vector<std::vector<int>> m{{1, 2, 3}, {4, 5, 6}};
    auto result = solution.minPathSum(m);
    std::cout << result << std::endl;
    assert(result == 12);
  }
}
