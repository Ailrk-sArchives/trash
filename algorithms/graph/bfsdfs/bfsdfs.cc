// src:
//    MIT algorithm book chapter 22

// A different way of thinking about breath first search.
// 1. white: default for all nodes at the beginning
// 2. black: nodes that are not connected with white
// 3. gray: the remainnig (connected both black and white)
//
// It's not necessary to acutally color the node though.
//
// Practice the standard solution.
// The speed and familarity is very important for these basic algorithms.

#include <deque>
#include <iostream>
#include <vector>
#include <algorithm>

using graph_t = std::vector<std::vector<int>>;

std::string graph_str = R"( graph
                             0 - 1 - 3
                             | / |
                             2 - 5 - 7 - 8 - 9
                             |   |   |
                             4   6 - 10 - 11 - 12 - 13 - 14 - 15 )";

graph_t graph = {
    {1},        {0, 2, 3},  {1, 3, 4, 5},      // 0 - 2
    {1, 2, 5},  {2},        {2, 3, 6, 7},      // 3 - 5
    {5, 10},    {5, 8, 10}, {7, 9},       {8}, // 6 - 9
    {6, 7, 11}, {10, 12},   {11, 13},          // 10 - 12
    {12, 14},   {13, 15},   {14}               // 13 - 15
};

std::string tree_str = R"(  tree
                                     0
                                   /   \
                                  1     2
                                 / \   / \
                                3   4 5   6
                               / \
                              7   8
                             /
                            9                 )";

graph_t tree = {{1, 2}, {0, 3, 4}, {0, 5, 6}, {1, 7, 8}, {1},
                {2},    {2},       {3, 9},    {3},       {7}};

std::vector<int> bfs(graph_t graph, int root) {
  std::deque<int> queue;
  std::vector<int> visited;
  queue.push_back(root);
  visited.push_back(root);

  while (queue.size() != 0) {
    int v = queue.front();
    queue.pop_front();
    for (auto &u : graph[v]) {
      auto it = std::find(visited.begin(), visited.end(), u);
      if (it == visited.end()) {
        visited.push_back(u);
        queue.push_back(u);
      }
    }
  }

  return visited;
}

std::vector<int> dfs(graph_t graph, int root) {
  std::vector<int> stack;
  std::vector<int> visited;

  stack.push_back(root);
  visited.push_back(root);

  while (stack.size() != 0) {
    int v = stack.back();
    stack.pop_back();
    for (auto &u : graph[v]) {
      auto it = std::find(visited.begin(), visited.end(), u);
      if (it == visited.end()) {
        stack.push_back(u);
        visited.push_back(u);
      }
    }
  }

  return visited;
}

void print_vec(const std::vector<int> &vec) {
  for (auto n : vec) {
    std::cout << n << ", ";
  }
  std::cout << "\n";
}

int main(void) {
  std::vector<int> res;
  std::cout << graph_str << std::endl;
  std::cout << "bfs with queue, start from 5" << std::endl;
  res = bfs(graph, 5);
  print_vec(res);
  std::cout << "dfs with stack, start from 5" << std::endl;
  res = dfs(graph, 5);
  print_vec(res);

  std::cout << "===================================" << std::endl;

  std::cout << tree_str << std::endl;
  std::cout << "bfs with queue, start from 0" << std::endl;
  res = bfs(tree, 0);
  print_vec(res);
  std::cout << "dfs with stack, start from 0" << std::endl;
  res = dfs(tree, 0);
  print_vec(res);

  return 0;
}
