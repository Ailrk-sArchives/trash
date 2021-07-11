#include <cassert>
#include <iostream>
#include <vector>

// another 2d dynamic programming

using namespace std;

// use dp top left to bottom right, and then bottom right to top left again
//
// also you can use depth first searth
class Solution {
public:
  vector<vector<int>> updateMatrix(vector<vector<int>> &mat) {
    int m = mat.size(), n = m ? mat[0].size() : 0;
    std::vector<std::vector<int>> dp(m, std::vector<int>(n, INT32_MAX - 1));

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (mat[i][j] == 0) {
          dp[i][j] = 0;
        } else {
          if (i > 0)
            dp[i][j] = std::min(dp[i][j], dp[i - 1][j] + 1);
          if (j > 0)
            dp[i][j] = std::min(dp[i][j], dp[i][j - 1] + 1);
        }
      }
    }

    for (int i = m - 1; i >= 0; --i) {
      for (int j = n - 1; j >= 0; --j) {
        if (mat[i][j] != 0) {
          if (i < m - 1)
            dp[i][j] = std::min(dp[i][j], dp[i + 1][j] + 1);
          if (j < n - 1)
            dp[i][j] = std::min(dp[i][j], dp[i][j + 1] + 1);
        }
      }
    }
    return dp;
  }
};

int main(void) {

  Solution solution;

  {
    std::vector<std::vector<int>> m{{0, 0, 0}, {0, 1, 0}, {0, 0, 0}};
    auto result = solution.updateMatrix(m);

    std::vector<std::vector<int>> ans = {{0, 0, 0}, {0, 1, 0}, {0, 0, 0}};
    assert(result == ans);
  }

  {
    std::vector<std::vector<int>> m{{0, 0, 0}, {0, 1, 0}, {1, 1, 1}};
    auto result = solution.updateMatrix(m);

    std::vector<std::vector<int>> ans = {{0, 0, 0}, {0, 1, 0}, {1, 2, 1}};
    assert(result == ans);
  }
  return 0;
}
