#include <algorithm>
#include <deque>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

// different types of graphs
// 0. GRAPH
//    a graph is denoted as G(V, E)
//    where V = {vetexes}
//          E = {edges}

// 1. DIRECTED / UNDIRECTED GRAPH, SIMPLE ? MULTI GRAPH
//
//   A-----B---------E
//   \     |\       /|
//    \    | \     / |
//     \   |  \   /  |
//      \  |   \ /   |
//        C.    D----F
//        this is a simple graph,
//        no self looping, no vertex has multiple edges.
//
//    +----+      +--+
//    A    B --- C  |
//    +----+      +--+  and this is a multigraph.

// -- a little exercise:
// Imagine a padlock start with 00. You can move 1 up to make 0
// a 1, 1 down to make 0 a 9.
// There are dead combinations that if padlock is in those state
// it is permanently locked. Find the minimum # of moves to reach
// a target combination without trigger dead combination
// :dead combination: [10, 90, 12], target 11
//
// solution:
//   1. first encode the initial state as a graph.
//      start from 00, there are 4 possible next states.
//        10
//         |
//    09--00--01
//         |
//        90
//
//   2.
//        10  11*
//         |  |
//    09--00--01--02
//         |  |
//        90  91

struct PadLockNode {
  unsigned d0 = 0;
  unsigned d1 = 0;
  unsigned steps = 0;
  static std::vector<std::pair<int, int>> dead_combinations;

  PadLockNode(unsigned d0, unsigned d1, int steps)
      : d0(d0), d1(d1), steps(steps + 1) {}
  PadLockNode() : d0(0), d1(0), steps(0) {}

  bool discovered_ = false;

  void set_dead_combination(const std::vector<std::pair<int, int>> combs) {
    // when there is dead combination just trim it.
    dead_combinations = combs;
  }

  bool is_allowed(std::pair<int, int> p) {
    return std::find_if(dead_combinations.begin(), dead_combinations.end(),
                        [&](auto dp) {
                          return p.first == dp.first && p.second == dp.second;
                        }) == dead_combinations.end();
  }

  auto adjacents() -> std::vector<PadLockNode> {
    std::vector<PadLockNode> buffer{};
    std::pair<int, int> pairs[4]{{(d0 + 1) % 10, d1},
                                 {(d0 - 1) % 10, d1},
                                 {d0, (d1 + 1) % 10},
                                 {d0, (d1 - 1) % 10}};
    for (auto p : pairs) {
      if (is_allowed(p)) {
        buffer.push_back(PadLockNode(p.first, p.second, steps));
      }
    }
    return buffer;
  }
};

std::vector<std::pair<int, int>> PadLockNode::dead_combinations{};

// do a breath first search
int find_parlock_target_bfs(PadLockNode root, int td0, int td1) {
  std::deque<PadLockNode> q;
  root.discovered_ = true;

  q.push_back(root);
  while (q.size() > 0) {

    auto v = q.front();
    q.pop_front();

    if (v.d0 == td0 && v.d1 == td1) {
      return v.steps;
    }

    for (auto w : v.adjacents()) {
      if (!w.discovered_) {
        w.discovered_ = true;
        q.push_back(w);
      }
    }
  }
  return -1;
}

void test_find_parlock_target() {
  std::cout << "---------------------" << std::endl;
  auto root = PadLockNode();
  root.set_dead_combination({{1, 0}, {9, 0}, {1, 2}});

  auto answer = find_parlock_target_bfs(root, 1, 1);
  std::cout << "minial steps: " << answer << std::endl;

  root = PadLockNode();
  root.set_dead_combination({{1, 0}, {0, 1}, {1, 2}});

  answer = find_parlock_target_bfs(root, 1, 1);
  std::cout << "minial steps: " << answer << std::endl;
}

// 2. REPRESENTATION OF GRAPH
//   1. nodes
//      Graph is a more generalized linked list, so just
//      create linked list nodes with some variations.
//      (next points to a list of nodes etc.)
//
//   2. adjacent lists, a list holds all vetexes, another holds
//      all edges.
//
//         C..
//         |  \..
//         |     \
//         A ---- B
//         |
//         |
//         D
//
//      V = [A, B, C, D]
//      E = [AB, AC, AD, CB]
//
//      This approach is more compact, but harder to traverse.
//      These is probably the closest implementation to the
//      G(V, E) definition.
//
//   3. dictionary

static std::unordered_map<char, std::vector<char>> dict_graph{
    {'A', {'B', 'C', 'D'}},
    {'B', {'A', 'C'}},
    {'C', {'A', 'B'}},
    {'D', {'A'}} // undirected graph with dictionary.
};

//   4. adjacency matrix
//      1. undirected unweighted.
//
//         C..
//         |  \..
//         |     \
//         A ---- B
//         |
//         |
//         D
//
//           A B C D
//         A 0 1 1 1
//         B 1 0 1 0
//         C 1 1 0 0
//         D 1 0 0 0
// ----------------------------------
//      2. directed unweighted
//
//         C<.
//         |  \..
//         V     \
//         A <--> B
//         |
//         V
//         D
//
//           A B C D
//         A 0 1 1 1
//         B 1 0 1 0
//         C 0 0 0 0
//         D 0 0 0 0
// ----------------------------------
//      3. undirected weighted
//
//         C.. 7
//       2 |  \..
//         |     \
//         A ---- B
//         |  1
//       9 |
//         D
//
//           A B C D
//         A _ 1 2 9
//         B 1 _ 7 _
//         C 2 7 _ _
//         D 9 _ _ _      :_ is inf
// ----------------------------------
//       4.  directed weighted
//
//         C<. 7
//       2 |  \..
//         V     \
//         A <--> B
//         |  1
//       9 V
//         D
//
//           A B C D
//         A 0 1 2 9
//         B 1 0 2 0
//         C 0 0 0 0
//         D 0 0 0 0
//
// THis is a undirected graph representation with
// adjecent matrix.
enum AdjNode { A = 0, B = 1, C, D };
static std::array<std::array<int, 4>, 4> adjmatrix{{
    {0, 1, 1, 1},
    {1, 0, 1, 0},
    {1, 1, 0, 0},
    {1, 0, 0, 0},
}};

char adjnode_char(AdjNode a) {
  switch (a) {
  case A:
    return 'A';
  case B:
    return 'B';
  case C:
    return 'C';
  case D:
    return 'D';
  }
}

// show if a is connected with b.
void adj_connected(AdjNode a) {
  std::cout << adjnode_char(a) << " -> { ";

  for (int i = A; i <= D; ++i) {
    if (adjmatrix[a][i] == 1) {
      std::cout << adjnode_char(static_cast<AdjNode>(i)) << ",";
    }
  }
  std::cout << " }"
            << "\n";
}

void test_adj_connected() {
  std::cout << "---------------------" << std::endl;
  adj_connected(A);
  adj_connected(B);
  adj_connected(C);
  adj_connected(D);
}

// 3. LOOPS IN GRAPH
//    1. cyclic graph
//      B -> E -> F -> G -> B
//      A -> B -> C -> D -> A
//      E -> F -> G -> E
//      B -> E -> G -> B
//      are cyclic
//      cyclic graph can implies the traversing
//      never end. That's why we normally neeed to
//      mark a node as visited to avoid doint
//      repeative work.
//
//      A --- B---E
//      |     |\  |\
//      |     | \ | \
//      D --- C  .G--F
//
//    2. acyclc graph
//      on the other hand, some graph don't have any
//      cycle at all, thus traverse will always terminate.
//      list and binary tree are two examples.
//
//      A -- B -- E -- F -- G
//           |
//           C -- D
//
//    3. walk, trail, path, cycle and circuit.
//      These are different names refer to different path
//      in a graph one can take.
//
//      Two predicates are important here.
//      e := edge can be repeated
//      v := vertex can be repeated
//
//      Given this graph:
//
//      A --- B---E
//      |     |\  |\
//      |     | \ | \
//      D --- C  .G--F
//
struct Walk {
  //      walk: e && v
  //        A -> B -> C -> B -> E
  bool need_closed = false;
  bool edge_can_repeat = true;
  bool vertex_can_repeat = true;
};

struct Trail {
  //      trail: !e && v
  //        E -> F -> G -> E -> B
  bool need_closed = false;
  bool edge_can_repeat = false;
  bool vertex_can_repeat = true;
};

struct Path {
  //      path: !e && !v
  //        A -> B -> E -> F
  bool need_closed = false;
  bool edge_can_repeat = false;
  bool vertex_can_repeat = false;
};

struct Circuit : public Trail {
  //      circuit: closed && !e && v
  bool need_closed = true;
};

struct Cycle : public Path {
  //      cycle: closed && !e && !v
  bool need_closed = true;
};

// 4. Some lemmas
//    1. Degree
//      degree of a vertex is the # of edges it's adjacent to.
//      denoted as deg(v)
//      a vertex of a directed graph has in degree deg-(v)
//      and out degree deg+(v)
//
//    note now you have three objects on the table.
//      Vertexes, Edges, and Degrees.
//
struct Node_D { // a node of directed graph.
  std::vector<Node_D *> adjacents_to;
  std::vector<Node_D *> adjacents_from;
  int deg_in() { return adjacents_from.size(); }
  int deg_out() { return adjacents_to.size(); }
  int deg() { return deg_in() + deg_out(); }
};

//    2. handshaking theorem
//        Edges and degrees are closely related by this lemma:
//        For undirected graph, each edge contribute to degree twice,
//
//            A == B
//
//          For a undirected graph,
//            Sum(deg(v)) =  2 |E|
//            v in V
//          This implies:
//            1. sum of degree is always even
//            2. known num of edges we know the sum of degrees.
struct Graph_D {
  std::vector<Node_D *> nodes;
  int sum_of_deg() { return nodes.size(); }
};

struct Node_N {
  std::vector<Node_N *> adjacents;
  int deg() { return adjacents.size(); }
};

struct Graph_N {
  std::vector<Node_N *> nodes;
  int sum_of_deg() { return nodes.size() * 2; }
};

//      3. lemma of handshaking theorem:
//        An undirected graph has even # of vertices of odd degrees.
//        Proof:
//          Let V1, V2 be the sets of vertices with even and odd degrees
//          respectively. we know Veven, Vodd can't overlap.
//          V = V1 union V2,
//
//          By handshaking  theorem,
//
//               Sum(deg(V)) = 2|E|
//           =>  2|E| = Sum(deg(Veven)) + Sum(deg(Vodd))
//
//           Sum(deg(V1)) must be even,
//             [proof:
//              let v1 in Veven, we know deg(v1) = 2k for k in Z.
//              => Sum(deg(Veven)) = Sum(2 . h) = 2 Sum(h) for h in Z.
//              done.
//             ]
//
//           So 2 | Sum(deg(Vodd))
//           let v2 in Vodd, deg(v2) is odd.
//           Sum(deg(v2)) = Sum(2k + 1) = Sum(2k) + Sum(1)
//           => 2 | Sum(1)
//           => There are even number of vertices with odd degree.

//      4. Complete graph
//        A complete graph is a simple graph that each vertex connected
//        with all other vertexes.
//
//            x ---- x
//
//            x ----- x
//             \     /
//              \   /
//               \ /
//                x
//
//        |E| = n(n+1) / 2
//

//     5. Hyper cube
//        n-dimensional analogus of a square
//
//
//           x------x          x----x
//          /|     /|          |    |
//         x-+----x |          |    |
//         | |    | |          x----x Q2
//         | x----+-x
//         |/     |/
//         x------x   Q3
//            | projects to 2d
//            V  (they bascially looks the same...)
//           x-----x
//          / \   / \
//         /   \ /   \
//        x-----x-----x
//         \   / \   /
//          \ /   \ /
//           x-----x
//
//       denoted as Qn
//
//       2^n vertices, each represent a n bit string.
//
//       |E| = 2^n
//
//

int main(void) {
  test_find_parlock_target();
  test_adj_connected();
  return 0;
}
