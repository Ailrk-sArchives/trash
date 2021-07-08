#include <cassert>
#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// https://leetcode.com/problems/number-of-provinces/
//    1 2 3
// 1 [1 1 0]
// 2 [1 1 0]
// 3 [0 0 1]

namespace stack_based {

class Solution {
public:
  int findCircleNum(vector<vector<int>> &isConnected) {
    int m = isConnected.size();
    int n = m ? isConnected[0].size() : 0;
    assert(m == n);
    int province_count = 0;

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (isConnected[i][j]) {
          ++province_count;
          isConnected[i][j] = 0;

          std::stack<int> s;
          s.push(i);
          while (!s.empty()) {
            auto e = s.top();
            s.pop();
            for (int k = 0; k < m; ++k) {
              if (isConnected[e][k]) {
                isConnected[e][k] = 0;
                s.push(k);
              }
            }
          }
        }
      }
    }
    return province_count;
  }
};
} // namespace stack_based

namespace recursion_based {
class Solution {
public:
  int findCircleNum(vector<vector<int>> &isConnected) {
    int m = isConnected.size();
    int n = m ? isConnected[0].size() : 0;
    assert(m == n);
    int province_count = 0;

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < m; ++j) {
        if (isConnected[i][j]) {
          ++province_count;
          dfs(isConnected, i);
        }
      }
    }
    return province_count;
  }

  void dfs(std::vector<std::vector<int>> &isConnected, int root) {
    int m = isConnected.size();

    for (int k = 0; k < m; ++k) {
      if (isConnected[root][k]) {
        isConnected[root][k] = 0;
        dfs(isConnected, k);
      }
    }
  }
};
} // namespace recursion_based

void test_case(auto solution) {

  {
    std::vector<std::vector<int>> v = {{1, 1, 0}, {1, 1, 0}, {0, 0, 1}};
    auto res = solution.findCircleNum(v);
    std::cout << res << std::endl;
    assert(res == 2);
  }

  {
    std::vector<std::vector<int>> v = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
    auto res = solution.findCircleNum(v);
    std::cout << res << std::endl;
    assert(res == 3);
  }
}

int main(void) {

  {
    using namespace stack_based;
    Solution solution;
    test_case(solution);
  }

  {
    using namespace recursion_based;
    Solution solution;
    test_case(solution);
  }

  return 0;
}
