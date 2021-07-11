#include <cassert>
#include <iostream>
#include <string>
#include <vector>
template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

using namespace std;
// 1 <= n < 25
// dp[i]: number of ways to decode at position i
// This problem looks like dp, but it's really about case analysis..
// So many edge cases to consider.
namespace sol1 {
class Solution {
public:
  int numDecodings(string s) {
    if (s.size() == 1)
      return valid(s.begin(), s.end());
    if (s[0] <= '0' || s[0] > '9')
      return 0;

    std::vector<int> dp(s.size(), 0);
    dp[0] = 1;

    dp[1] = valid(s.begin(), s.begin() + 2) ? s[1] != '0' ? 2 : 1
            : s[1] != '0'                   ? 1
                                            : 0;

    for (int i = 2; i < s.size(); ++i) {
      if (valid(s.begin() + i - 1, s.begin() + i + 1)) { // last two are valid
        dp[i] = (s[i] != '0') ? dp[i - 1] + dp[i - 2] : dp[i - 2];
      } else {
        dp[i] = (s[i] != '0')                               ? dp[i - 1]
                : (valid(s.begin() + i - 2, s.begin() + i)) ? dp[i - 2] - 1
                                                            : 0;
      }
    }
    return dp[s.size() - 1];
  }

  template <typename Iter> bool valid(Iter first, Iter last) {
    assert(last - first <= 2);
    if (*first == '0')
      return false;
    int n = std::stoi(std::string(first, last));
    return n > 0 && n <= 26;
  }

  template <typename Iter> bool valid(Iter index) {
    int n = *index;
    return n > 0 && n <= 26;
  }
};
} // namespace sol1

namespace sol2 {
class Solution {
public:
  int numDecodings(string s) {
    int n = s.length();
    if (n == 0)
      return 0;
    int prev = s[0] - '0';
    if (!prev)
      return 0;
    if (n == 1)
      return 1;
    std::vector<int> dp(n + 1, 1);

    for (int i = 2; i <= n; ++i) {
      int cur = s[i - 1] - '0';
      if ((prev == 0 || prev > 2) && cur == 0) {
        return 0;
      }

      if ((prev < 2 && prev > 0) || prev == 2 && cur < 7) {
        if (cur) {
          dp[i] = dp[i - 2] + dp[i - 1];
        } else {
          dp[i] = dp[i - 2];
        }
      } else {
        dp[i] = dp[i - 1];
      }
      prev = cur;
    }
    return dp[n];
  }
};
} // namespace sol2

int test_case(auto solution) {

  std::vector<std::pair<std::string, int>> cases{
      {"12", 2},    {"22", 2}, {"226", 3}, {"06", 0},   {"10", 1},
      {"0", 0},     {"1", 1},  {"27", 1},  {"2101", 1}, {"1123", 5},
      {"10011", 0}, {"30", 0}, {"301", 0}, {"13904", 0}};

  for (auto &[s, a] : cases)

  {
    std::cout << "[test] str: " << s << ", expect: " << a << std::endl;
    auto result = solution.numDecodings(s);
    std::cout << result << std::endl;
    assert(result == a);
  }

  return 0;
}

int main(void) {

  {
    using namespace sol1;
    Solution solution;
    test_case(solution);
  }

  {
    using namespace sol2;
    Solution solution;
    test_case(solution);
  }
  return 0;
}
