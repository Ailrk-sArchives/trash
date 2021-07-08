#include "417-data-only.h"
#include <cassert>
#include <iostream>
#include <set>
#include <unordered_map>
#include <vector>
using namespace std;

template <typename C> void print_grid(C &&container) {
  if (container.empty()) {
    std::cout << "empty" << std::endl;
    return;
  }
  for (auto &n : container) {
    for (auto &s : n)
      std::cout << s << " ";
    std::cout << std::endl;
  }
  std::cout << std::endl;
}

// with root n, we consider there is an edge if any nodes next to it has
// value smaller or equals to heights[n].
// dfs to find all nodes in the graph
// We can simply use memoization and store flow set already computed.

namespace sol1 {
// search from peacks to oceans. We need to do dfs at every position, which can
// have a high time complexity. To make it run faster we need to prun
// unecessary cases.
class Solution {
public:
  enum Flow {
    Blocked = -1,
    Pacific = 1,
    Atlantic,
  };

  struct pair_hash {
    template <typename T1, typename T2>
    std::size_t operator()(const std::pair<T1, T2> &p) const {
      return std::hash<T1>{}(p.first) ^ std::hash<T2>{}(p.second);
    }
  };

  std::vector<int> direction{-1, 0, 1, 0, -1};

  vector<vector<int>> pacificAtlantic(vector<vector<int>> &heights) {
    const int m = heights.size(), n = m ? heights[0].size() : 0;
    std::vector<std::vector<int>> result;
    std::unordered_map<std::pair<int, int>, int, pair_hash> visited;
    std::unordered_map<std::pair<int, int>, std::set<Flow>, pair_hash> memo;

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        visited.clear();
        auto flows = dfs(heights, visited, memo, {i, j});
        if (flows.find(Atlantic) != flows.end() &&
            flows.find(Pacific) != flows.end())
          result.push_back({i, j});
      }
    }
    return result;
  }

  std::set<Flow>
  dfs(std::vector<std::vector<int>> &heights,
      std::unordered_map<std::pair<int, int>, int, pair_hash> &visited,
      std::unordered_map<std::pair<int, int>, std::set<Flow>, pair_hash> &memo,
      std::pair<int, int> root) {
    const int m = heights.size(), n = m ? heights[0].size() : 0;

    auto &[i, j] = root;
    visited[{i, j}] = true;

    /* if (!memo[{i, j}].empty()) { */
    /*   return memo[{i, j}]; */
    /* } */

    std::set<Flow> flow;
    for (int k = 0; k < 4; ++k) {
      int x = i + direction[k];
      int y = j + direction[k + 1];
      if (visited[{x, y}]) {
        flow.insert(Blocked);
        continue;
      }

      if (x < 0 || y < 0) {
        flow.insert(Pacific);
      } else if (x >= m || y >= n) {
        flow.insert(Atlantic);
      } else if (heights[x][y] <= heights[i][j]) {
        auto subflow = memo[{x, y}].empty()
                           ? dfs(heights, visited, memo, {x, y})
                           : memo[{x, y}];
        // auto subflow = dfs(heights, visited, memo, {x, y});
        flow.insert(subflow.begin(), subflow.end());
      } else {
        flow.insert(Blocked);
      }
    }

    memo[{i, j}].insert(flow.begin(), flow.end());
    return flow;
  }
};
} // namespace sol1

namespace sol2 {
// search from oceans to peaks
// this solution is much clearer
class Solution {
public:
  std::vector<int> direction{-1, 0, 1, 0, -1};

  vector<vector<int>> pacificAtlantic(vector<vector<int>> &heights) {
    if (heights.empty() || heights[0].empty())
      return {};
    const int m = heights.size(), n = heights[0].size();
    std::vector<std::vector<int>> result;

    std::vector<std::vector<bool>> can_reach_pacific(
        m, std::vector<bool>(n, false));
    std::vector<std::vector<bool>> can_reach_atlantic(
        m, std::vector<bool>(n, false));

    for (int i = 0; i < m; ++i) {
      dfs(heights, can_reach_pacific, {i, 0});
      dfs(heights, can_reach_atlantic, {i, n - 1});
    }

    for (int i = 0; i < n; ++i) {
      dfs(heights, can_reach_pacific, {0, i});
      dfs(heights, can_reach_atlantic, {m - 1, i});
    }

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (can_reach_pacific[i][j] && can_reach_atlantic[i][j])
          result.push_back({i, j});
      }
    }
    return result;
  }

  void dfs(const std::vector<std::vector<int>> &heights,
           std::vector<std::vector<bool>> &can_reach,
           std::pair<int, int> root) {
    auto &[i, j] = root;
    if (can_reach[i][j]) {
      return;
    }
    can_reach[i][j] = true;
    for (int k = 0; k < 4; ++k) {
      int x = i + direction[k], y = j + direction[k + 1];
      if (x >= 0 && x < heights.size() && y >= 0 && y < heights[0].size() &&
          heights[i][j] <= heights[x][y]) {
        dfs(heights, can_reach, {x, y});
      }
    }
  }
};
} // namespace sol2

void test_case(auto solution) {
  {
    std::vector<std::vector<int>> heights{{1, 2, 2, 3, 5},
                                          {3, 2, 3, 4, 4},
                                          {2, 4, 5, 3, 1},
                                          {6, 7, 1, 4, 5},
                                          {5, 1, 1, 2, 4}};

    std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);

    print_grid(paths);
    std::vector<std::vector<int>> ans = {{0, 4}, {1, 3}, {1, 4}, {2, 2},
                                         {3, 0}, {3, 1}, {4, 0}};
    assert(ans == paths);
  }

  {
    std::vector<std::vector<int>> heights{{2, 1}, {1, 2}};
    std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);

    print_grid(paths);
    std::vector<std::vector<int>> ans = {{0, 0}, {0, 1}, {1, 0}, {1, 1}};
    assert(ans == paths);
  }

  {
    std::vector<std::vector<int>> heights{{1, 2, 3, 4},    //
                                          {12, 13, 14, 5}, //
                                          {11, 16, 15, 6}, //
                                          {10, 9, 8, 7}};  //
    std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);

    print_grid(paths);
    std::vector<std::vector<int>> ans{{0, 3}, {1, 0}, {1, 1}, {1, 2}, {1, 3},
                                      {2, 0}, {2, 1}, {2, 2}, {2, 3}, {3, 0},
                                      {3, 1}, {3, 2}, {3, 3}};
    assert(ans == paths);
  }

  {
    std::cout << "Big island: " << std::endl;
    std::vector<std::vector<int>> heights BIG_ISLAND;
    std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);

    print_grid(paths);
  }

  {
    std::cout << "Big island1: " << std::endl;
    std::vector<std::vector<int>> heights BIG_ISLAND1;
    std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
    print_grid(paths);
  }
}

int main() {
  {
    using namespace sol1;
    Solution solution;
    test_case(solution);
  }

  /* { */
  /*   using namespace sol2; */
  /*   Solution solution; */
  /*   test_case(solution); */
  /* } */
}
