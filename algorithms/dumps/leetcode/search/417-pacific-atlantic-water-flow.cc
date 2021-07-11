#include "417-data-only.h"
#include <cassert>
#include <csignal>
#include <iostream>
#include <set>
#include <unordered_map>
#include <vector>
using namespace std;
#define DEBUG

#ifdef DEBUG
int total = 0;
int memoized = 0;
int indent_num = 0;

void indent(bool inc) {
  if (inc)
    indent_num++;
  for (int i = 0; i < indent_num; ++i)
    std::cout << "    ";
}

void unindent() { indent_num--; }

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
#endif

// with root n, we consider there is an edge if any nodes next to it has
// value smaller or equals to heights[n].
// dfs to find all nodes in the graph
// We can simply use memoization and store flow set already computed.

namespace sol1 {
class Solution {
  // TODO handle loop case!
  // pruning is really annoying.
public:
  unsigned int can_reach_pacific = 0b011;
  unsigned int can_reach_atlantic = 0b101;
  unsigned int blocked = 0b001;
  unsigned int loop = 0b100;
  unsigned int in_progress = 0b010;

  std::vector<int> direction{-1, 0, 1, 0, -1};
  std::vector<std::vector<unsigned int>> state;

  vector<vector<int>> pacificAtlantic(vector<vector<int>> &heights) {
    int m = heights.size();
    int n = m ? heights[0].size() : 0;
    std::vector<std::vector<int>> result;
    state = std::vector<std::vector<unsigned int>>(
        m, std::vector<unsigned int>(n, 0));

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        std::vector<std::vector<bool>> visited(m, std::vector<bool>(n, false));

        auto flow = dfs(heights, visited, i, j);
        if (flow == 0b111)
          result.push_back({i, j});
      }
    }
    return result;
  }

  unsigned int dfs(std::vector<std::vector<int>> &heights,
                   std::vector<std::vector<bool>> &visited, int i, int j) {
    const int m = heights.size(), n = m ? heights[0].size() : 0;

    if (visited[i][j]) {
      if (state[i][j] == in_progress)
        return in_progress; // to signal the caller.
      else
        return state[i][j] || blocked;
    }

    visited[i][j] = true;

    // handle loop later.
    if (state[i][j] != 0 && state[i][j] != loop) {
      return state[i][j];
    }

    unsigned int flow;
    assert(state[i][j] == loop || state[i][j] == 0);
    state[i][j] |= in_progress; // mark as in progress 0b110

    // marki possible loops.
    // if more then two neighbours have the same height, mark later ones
    // as loop.
    for (int k = 0, h = 0; k < 4; ++k) {
      int x = i + direction[k], y = j + direction[k + 1];
      if (x >= 0 && x < m && y >= 0 && y < n &&
          heights[x][y] == heights[i][j]) {
        ++h;
        state[x][y] |= h > 1 ? loop : 0;
      }
    }

    // recursively test if neighbours can reach eitehr ocean.
    for (int k = 0; k < 4; ++k) {
      int x = i + direction[k], y = j + direction[k + 1];
      auto mask = (x < 0 || y < 0)     ? can_reach_pacific
                  : (x >= m || y >= n) ? can_reach_atlantic
                  : (heights[x][y] <= heights[i][j])
                      ? dfs(heights, visited, x, y)
                      : blocked;

      // handle loop
      // mask can be loop | in_progress
      // state can only be loop or 0
      if (mask & in_progress && state[i][j] & loop) {
        mask = blocked;
        visited[i][j] = false; // traverse this node again later
      }
      flow |= mask;
    }
    state[i][j] = flow;
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

void test1(auto solution) {
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

void test2(auto solution) {
  std::vector<std::vector<int>> heights{{2, 1}, {1, 2}};
  std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);

  print_grid(paths);
  std::vector<std::vector<int>> ans = {{0, 0}, {0, 1}, {1, 0}, {1, 1}};
  assert(ans == paths);
}

void test3(auto solution) {
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

void test4(auto solution) {
  std::cout << "Big island: " << std::endl;
  std::vector<std::vector<int>> heights BIG_ISLAND;
  std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
  print_grid(paths);
}

void test5(auto solution) {
  std::cout << "Big island1: " << std::endl;
  std::vector<std::vector<int>> heights BIG_ISLAND1;
  std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
  print_grid(paths);
}

void test6(auto solution) {
  std::vector<std::vector<int>> heights{{1, 1}, {1, 1}, {1, 1}};
  std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
  std::cout << std::endl;
  print_grid(paths);
  std::vector<std::vector<int>> ans = {{0, 0}, {0, 1}, {1, 0},
                                       {1, 1}, {2, 0}, {2, 1}};
  assert(ans == paths);
}

void test7(auto solution) {
  std::cout << "test 7" << std::endl;
  std::vector<std::vector<int>> heights{
      {10, 10, 10}, {10, 1, 10}, {10, 10, 10}};
  std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
  std::cout << std::endl;
  print_grid(paths);
  std::vector<std::vector<int>> ans = {{0, 0}, {0, 1}, {0, 2}, {1, 0},
                                       {1, 2}, {2, 0}, {2, 1}, {2, 2}};
  assert(ans == paths);
}

int main() {
  using namespace sol2;
  // using namespace sol2;

  Solution solution;
  test1(solution);
  test2(solution);
  test3(solution);
  test4(solution);
  test5(solution);

  // loop case
  test6(solution);
  test7(solution);
}
