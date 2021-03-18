#include <iostream>
#include <limits>
#include <vector>

// TODO

// to find a minimum cost from a source to sink in a multistage graph.
// a multistage graph is a directed graph with a node with in degree 0 known as
// source, and a node with out degree 0 as sink.
// typically we have k-stage where k is then number of stage of the graph.
//
// A property of multistage set is that vertexes are partitioned into k sets.
// when visiting a node in set ki, we know nodes in k-1 must be visited already.

// we use 2d vector to represent the graph as adjacent table.
// index is the node, the vector contains nodes it connected to and the weight
// of the edge.
// An assumption is veretices are orderd by stage. e.g idx 0 is the source and
// idx size() - 1 is the sink.

struct stage_graph {
  std::vector<std::vector<std::pair<int, int>>> g;
  int k;
};

void pprint(const std::vector<std::pair<int, int>> &v) {
  for (auto &[i, w] : v) {
    std::cout << "(" << i << ", " << w << ")"
              << ", ";
  }
  std::cout << "\n";
}

#define MAX 10000
#define min(a, b) (a > b ? b : a)

std::pair<int, int> find_min(int j, const stage_graph &graph, int *cost) {

  int r = -1;
  int c = MAX;

  for (auto &[v, w] : graph.g[j]) {
    int cd = c;
    c = min(c, w + cost[v]);
    r = c == cd ? r : v;
  }
  std::cout << "j: " << j << "r: " << r << "c: " << c << std::endl;
  return {r, c};
}

std::vector<int> multistage(const stage_graph &graph) {
  int n = graph.g.size();
  int k = graph.k;
  int cost[n];
  int d[n];
  std::vector<int> p(k, 0);
  for (int i = 0; i < n; ++i) {
    cost[i] = 0;
  }

  for (int j = n - 1; j != 0; --j) { // exclude sink

    // find vertex r s.t (j, r) in graph, and c(j, r) + cost(r) is minimal.
    auto [r, c] = find_min(j, graph, cost);
    cost[j] = c;
    d[j] = r;
  }

  p[1] = 1;
  p[k - 1] = n;
  for (int j = 1; j < k - 2; ++j) {
    p[j] = d[p[j - 1]];
  }
  return p;
}

#define TEST
#ifdef TEST
stage_graph graph = {.g =
                         {
                             {{1, 1}, {2, 2}, {3, 5}}, // 0

                             {{4, 4}, {5, 11}},         // 1
                             {{4, 9}, {5, 5}, {6, 16}}, // 2
                             {{6, 2}},                  // 3

                             {{7, 18}}, // 4
                             {{7, 13}}, // 5
                             {{7, 2}},  // 6

                             // 7
                         },
                     .k = 4};

int main(void) {

  std::cout << "grap: " << std::endl;
  for (auto v : graph.g) {
    pprint(v);
  }

  std::vector<int> path = multistage(graph);

  std::cout << "path: " << std::endl;
  for (auto v : path) {
    std::cout << v << ", ";
  }
  std::cout << "\n";

  return 0;
}

#endif
