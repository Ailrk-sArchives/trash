#include <cassert>
#include <cmath>
#include <iostream>
#include <queue>
#include <unordered_set>
#include <vector>

using namespace std;

// cutting problem
// n = Sum(ps) where ps = list of perfect number

namespace dp_solution {

// solve with dp
// dp[i] be the minimum number of perfect square numbers needed to form i.
// dp[i] = min(dp[i - 2], dp[i - 4], ...) + 1

class Solution {
public:
  int numSquares(int n) {
    if (n <= 0) return 0;
    std::vector<int> dp(n + 1, INT32_MAX);
    dp[0] = 0; // base case

    for (int i = 1; i <= n; ++i) {
      for (int j = 1; j * j <= i; ++j) {
        dp[i] = std::min(dp[i], dp[i - j * j] + 1);
      }
    }
    return dp[n];
  }
};
} // namespace dp_solution

namespace bfs_solution {
// solution with breath first search
class Solution {
public:
  int bfs(int n) {
    std::queue<std::pair<int, int>> q;
    int num, height, i;
    std::unordered_set<int> visited;
    q.push({n, 0});
    while (!q.empty()) {
      num = q.front().first;
      height = q.front().second;
      q.pop();

      if (num == 0)
        return height;
      if (visited.find(num) != visited.end())
        continue;
      int l = sqrt(num);
      for (int i = l; i >= 1; --i) {
        q.push({num - i * i, height + 1});
      }
    }
    return 0;
  }

  int numSquares(int n) { return bfs(n); }
};

} // namespace bfs_solution

int test_case(auto solution) {

  {
    // 4 + 4 + 4
    int n = 12;
    int result = solution.numSquares(n);
    assert(result == 3);
  }

  {
    // 9 + 4
    int n = 13;
    int result = solution.numSquares(n);
    assert(result == 2);
  }
  return 0;
}

int main(void) {

  {
    using namespace dp_solution;
    Solution solution;
    test_case(solution);
  }

  {
    using namespace bfs_solution;
    Solution solution;
    test_case(solution);
  }
  return 0;
}
