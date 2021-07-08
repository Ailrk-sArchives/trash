#include <cassert>
#include <iostream>
#include <set>
#include <unordered_map>
#include <vector>
using namespace std;

template <typename C> void print_grid(C &&container) {
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
//

namespace sol1 {
class Solution {
public:
  enum Flow {
    Pacific,
    Atlantic,
    Blocked,
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

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        std::cout << "\nSEARCH IN " << i << " " << j;
        std::cout << std::endl;
        visited.clear();
        auto flows = dfs(heights, visited, {i, j});
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
      std::pair<int, int> root) {
    const int m = heights.size();
    const int n = m ? heights[0].size() : 0;

    auto &[i, j] = root;
    std::set<Flow> flow;
    for (int k = 0; k < 4; ++k) {
      int x = i + direction[k];
      int y = j + direction[k + 1];
      if (visited[{x, y}]) {
        std::cout << "  skipped at # " << k << ", " << x << " " << y
                  << std::endl;
        continue;
      }

      std::cout << "  neighbour # " << k << ", " << x << " " << y << "     - {";
      for (auto &n : flow) {
        std::cout << n << " ";
      }
      std::cout << " } " << std::endl;

      visited[{x, y}] = true;
      if (x < 0 || y < 0) {
        flow.insert(Pacific);
      } else if (x >= m || y >= n) {
        flow.insert(Atlantic);
      } else if (heights[x][y] <= heights[i][j]) {
        std::cout << "    >>recurse!" << std::endl;
        auto subflow = dfs(heights, visited, {x, y});
        flow.insert(subflow.begin(), subflow.end());
        std::cout << "    <<recurse end" << std::endl;
      } else {
        flow.insert(Blocked);
      }
    }

    return flow;
  }
};
} // namespace sol1

namespace sol2 {
class Solution {
  vector<vector<int>> pacificAtlantic(vector<vector<int>> &heights) {
    // TODO simpmler solution
  }
};
} // namespace sol2

void test_case(auto solution) {
  /* { */
  /*   std::vector<std::vector<int>> heights{{1, 2, 2, 3, 5}, */
  /*                                         {3, 2, 3, 4, 4}, */
  /*                                         {2, 4, 5, 3, 1}, */
  /*                                         {6, 7, 1, 4, 5}, */
  /*                                         {5, 1, 1, 2, 4}}; */

  /*   std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
   */
  /*   print_grid(paths); */
  /*   std::vector<std::vector<int>> ans = {{0, 4}, {1, 3}, {1, 4}, {2, 2}, */
  /*                                        {3, 0}, {3, 1}, {4, 0}}; */
  /*   assert(ans == paths); */
  /* } */

  /* { */
  /*   std::vector<std::vector<int>> heights{{2, 1}, {1, 2}}; */
  /*   std::vector<std::vector<int>> paths = solution.pacificAtlantic(heights);
   */
  /*   print_grid(paths); */
  /*   std::vector<std::vector<int>> ans = {{0, 0}, {0, 1}, {1, 0}, {1, 1}}; */
  /*   assert(ans == paths); */
  /* } */

  {
    // TODO this test doesn't pass
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
}

int main() {
  {
    using namespace sol1;
    Solution solution;
    test_case(solution);
  }
}
