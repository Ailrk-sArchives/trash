#include <cassert>
#include <iostream>
#include <stack>
#include <vector>
// https://leetcode.com/problems/max-area-of-island/
using namespace std;

// find the maximum are of an island in a grid
// island looks like this
// island must be 4 connected
// [[0,0,1,0,0,0,0,1,0,0,0,0,0],
//  [0,0,0,0,0,0,0,1,1,1,0,0,0],
//  [0,1,1,0,1,0,0,0,0,0,0,0,0],
//  [0,1,0,0,1,1,0,0,1,0,1,0,0],
//  [0,1,0,0,1,1,0,0,1,1,1,0,0],
//  [0,0,0,0,0,0,0,0,0,0,1,0,0],
//  [0,0,0,0,0,0,0,1,1,1,0,0,0],
//  [0,0,0,0,0,0,0,1,1,0,0,0,0]]

namespace stack_based {

class Solution {
public:
  std::vector<int> direction{-1, 0, 1, 0, -1};
  int maxAreaOfIsland(vector<vector<int>> &grid) {
    int m = grid.size();
    int n = m ? grid[0].size() : 0;
    int max_area = 0;

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (grid[i][j]) {
          int local_area = 1;
          grid[i][j] = 0;

          std::stack<std::pair<int, int>> island;
          island.push({i, j});
          while (!island.empty()) {
            auto adjacents = get_neighbours(grid, island.top());
            island.pop();
            for (auto &n : adjacents) {
              grid[n.first][n.second] = 0;
              ++local_area;
              island.push(n);
            }
          }
          max_area = std::max(max_area, local_area);
        }
      }
    }
    return max_area;
  }

  std::vector<std::pair<int, int>>
  get_neighbours(std::vector<std::vector<int>> &grid,
                 std::pair<int, int> &node) {
    // (direciotn[i], direction[i + 1]) specify the neighbor location
    constexpr static int direction[5] = {-1, 0, 1, 0, -1};

    std::vector<std::pair<int, int>> result;
    auto &[r, t] = node;
    for (int i = 0; i < 4; ++i) {
      int x = r + direction[i];
      int y = t + direction[i + 1];
      if (x >= 0 && x < grid.size() && y >= 0 && y < grid[0].size() &&
          grid[x][y] == 1) {
        result.push_back({x, y});
      }
    }
    return result;
  }
};

} // namespace stack_based

namespace recursion_based {
class Solution {
public:
  std::vector<int> direction{-1, 0, 1, 0, -1};

  int maxAreaOfIsland(vector<vector<int>> &grid) {
    if (grid.empty() || grid[0].empty())
      return 0;
    int max_area = 0;
    for (int i = 0; i < grid.size(); ++i) {
      for (int j = 0; j < grid[0].size(); ++j) {
        if (grid[i][j] == 1) {
          max_area = std::max(max_area, dfs(grid, i, j));
        }
      }
    }
    return max_area;
  }

  int dfs(std::vector<std::vector<int>> &grid, int r, int t) {

    if (grid[r][t] == 0)
      return 0;
    grid[r][t] = 0;
    int area = 1;
    for (int i = 0; i < 4; ++i) {
      int x = r + direction[i], y = t + direction[i + 1];
      if (x >= 0 && x < grid.size() && y >= 0 && y < grid[0].size())
        area += dfs(grid, x, y);
    }
    return area;
  }
};
} // namespace recursion_based

int test_case(auto solution) {

  {
    std::vector<std::vector<int>> island{
        {0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0},
        {0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0},
        {0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0},
        {0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0},
        {0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}};
    int v = solution.maxAreaOfIsland(island);
    std::cout << v << std::endl;
    assert(v == 6);
  }

  {
    std::vector<std::vector<int>> island{{0, 0, 0, 0, 0, 0, 0, 0}};
    int v = solution.maxAreaOfIsland(island);
    std::cout << v << std::endl;
    assert(v == 0);
  }

  {
    std::vector<std::vector<int>> island{{1, 1}, {1, 0}};
    int v = solution.maxAreaOfIsland(island);
    std::cout << v << std::endl;
    assert(v == 3);
  }

  return 0;
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
