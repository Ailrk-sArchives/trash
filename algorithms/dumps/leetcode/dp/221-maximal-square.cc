#include <cassert>
#include <iostream>
#include <vector>

using namespace std;

// in a 0 1 matrix
// find largest square containing only 1s.

// 0 0 1 0      0 0 1 0
// 0 1 1 1      0 1 1 1
// 1 1 1 1  =>  1 1 2 2
// 0 1 1 1      0 1 2 3
//
// 0 1 1 1      0 1 1 1
// 0 1 1 1      0 1 2 2
// 1 1 1 1  =>  1 1 2 3
// 0 1 0 1      0 1 0 1
//
// dp[i][j] represents the are of square that with it as bottom right coner.
// dp[i][j] = mat[i][j] == 0 ? 0 :
//                (dp[i - 1][j - 1] && dp[i -1][j] && dp[i][j - 1]) ?
//                  mat[i][j] + dp[i-1][j-1]

class Solution {
public:
  int maximalSquare(vector<vector<char>> &matrix) {
    int m = matrix.size(), n = m ? matrix[0].size() : 0;
    int max = INT32_MIN;

    std::vector<std::vector<int>> dp(m, std::vector<int>(n, 0));
    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (matrix[i][j] == '1') {
          dp[i][j] = (i - 1 < 0 || j - 1 < 0)
                         ? 1
                         : std::min(dp[i - 1][j - 1],
                                    std::min(dp[i][j - 1], dp[i - 1][j])) +
                               1;
        }
        max = std::max(max, dp[i][j]);
      }
    }
    return max * max;
  }
};

int main(void) {
  Solution solution;
  {
    std::vector<std::vector<char>> mat{{'1', '0', '1', '0', '0'},
                                       {'1', '0', '1', '1', '1'},
                                       {'1', '1', '1', '1', '1'},
                                       {'1', '0', '0', '1', '0'}};

    auto result = solution.maximalSquare(mat);
    std::cout << result << std::endl;
    assert(result == 4);
  }

  {
    std::vector<std::vector<char>> mat{{'0', '1'}, {'1', '0'}};

    auto result = solution.maximalSquare(mat);
    std::cout << result << std::endl;
    assert(result == 1);
  }

  return 0;
}
