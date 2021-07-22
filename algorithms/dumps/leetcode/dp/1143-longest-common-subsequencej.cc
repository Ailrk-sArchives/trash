#include <cassert>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// find the longest common subsequence
// t1 = a b c d e
// t2 = a c e
// The table represent the function encoded by the 2D array.
// Notice the diagnal line represents when index of t1 and t2 are the same.
// We can think traverse over j as incrementing t2 but keey at the same position
// in t1.
// i is the other way around.
//   | _ a b c d e
// --+-----------
// _ | 0 0 0 0 0 0
// a | 0 1 1 1 1 1
// c | 0 1 1 2 2 1
// e | 0 1 1 1 2 3
//
// if text1[i][j] == text2[i][j], we can increment the longest subsequence by 1
// otherwise we remain the same maximum subsequence of where we came from.
//
// Very common technique is to pad 0 in boundaries to be base cases.

class Solution {
public:
  int longestCommonSubsequence(string text1, string text2) {
    int m = text1.size(), n = text2.size();
    std::vector<std::vector<int>> dp(m + 1, std::vector<int>(n + 1, 0));

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (text1[i - 1] == text2[j - 1]) {
          dp[i][j] = dp[i - 1][j - 1] + 1;
        } else {
          dp[i][j] = std::max(dp[i][j - 1], dp[i - 1][j]);
        }
      }
    }

    return dp[m][n];
  }
};

int test_case(auto solution) {
  std::vector<std::pair<std::pair<std::string, std::string>, int>> problems{
      {{"abcde", "ace"}, 3}, {{"abc", "abc"}, 3}, {{"abc", "def"}, 0}};

  for (auto &[ts, a] : problems) {
    auto result = solution.longestCommonSubsequence(ts.first, ts.second);
    std::cout << result << std::endl;
    assert(result == a);
  }

  return 0;
}

int main(void) {
  Solution solution;
  test_case(solution);
  return 0;
}
