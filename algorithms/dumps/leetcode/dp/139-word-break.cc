#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;

// give a string and a dictionary, return true if s can be segmented into space
// separated sequence of one or more dictionary words.

namespace sol1 {

class Solution {
public:
  bool wordBreak(string s, vector<string> &wordDict) {
    std::vector<int> dp(s.size() + 1, false);
    int n = s.size();
    dp[0] = true;

    for (int i = 1; i <= n; ++i) {
      for (auto &word : wordDict) {
        int len = word.size();
        if (i >= len && word == s.substr(i - len, len)) {
          dp[i] = dp[i] || dp[i - len];
        }
      }
    }
    return dp[n];
  }
};

} // namespace sol1

int test_case(auto solution) {

  {
    std::vector<std::string> dict{"leet", "code"};
    auto result = solution.wordBreak("leetcode", dict);
    std::cout << result << std::endl;
    assert(result);
  }

  {
    std::vector<std::string> dict{"apple", "pen"};
    auto result = solution.wordBreak("applepen", dict);
    std::cout << result << std::endl;
    assert(result);
  }

  {
    std::vector<std::string> dict{"cats", "dog", "sand", "and", "cat"};
    auto result = solution.wordBreak("catsandog", dict);
    std::cout << result << std::endl;
    assert(!result);
  }

  return 0;
}

int main(void) {
  {
    using namespace sol1;
    Solution solution;
    test_case(solution);
  }

  return 0;
}
