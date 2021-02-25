#include "template-list.hpp"
#include <iostream>

// template topological sort
// get a total ordered list (topological order) from a graph without cycle (a
// DAG). note a DAG might have multiple topological ordering.
//
// A topological orerding can be useful for scheduling dependencies. So notable
// examples: dependeneis management of makefiles, resolving symbols in
// linkers...
//
// Given a graph:
// 5 -> 11, 7 -> 11, 7 -> 8
// 3 -> 8, 3 -> 10
// 11 -> 2, 11 -> 9, 11 -> 10, 8 -> 9
//
// Possible topological order:
//  5 7 3 11 8 2 9 10
//  3 5 7 8 11 2 9 10
//  ...
//  All verteices are reached after their dependencies are reached.
//
// Typical topo sort running type O(|V| + |E|).
//
// Two algorithms:
//    1. Kahn's algorithm
//    2. DFS
//

// Some list utility

// declare the graph.
template <typename...> struct Graph;

template <typename N> struct NodeWrapper {
  void operator()() { return N(); }
};

// Edge of a graph. An edge consists two nodes.
// @params:
//   Node1: typename, the first node
//   Node2: typename, the second node.
// @operations:
//   Out: typename, get the out node (first node.)
//   In:  typename, get the in node (second node.)
template <typename> struct Edge {};
template <typename Node1, typename Node2>
struct Edge<auto (*)(Node1)->auto (*)(Node2)->void> {
  using Out = Node1;
  using In = Node2;
};

// Get out node of an edge
template <typename Edge> struct GetOut { using type = typename Edge::Out; };

// Get in node of an edge
template <typename Edge> struct GetIn { using type = typename Edge::In; };

// The graph.
// @param:
//  Links: typename..., links of the graph. node it should be of type
//    auto(*)(a) -> auto(*)(b)
// @operations:
template <typename... Links> class Graph {
private:
  using Edges = typename Unique<List<Edge<Links>...>>::type;

  using OutNodes = typename Map<GetOut, Links...>::type;
  using InNodes = typename Map<GetIn, Links...>::type;

  // all nodes.
  using Nodes = typename Unique<typename Concat<InNodes, OutNodes>::type>::type;
  static constexpr size_t number_of_nodes = Nodes::size;

  // find dependencies.
  // Node -> { Node, List<Node> }

  template <typename N> class GetNodeDescedants {
  private:
    template <typename Edge>
    //  get all edges with `Node` as the out end.
    struct DependencyP : std::is_same<N, typename GetOut<Edge>::type> {};
    using NodeDescendants = typename Filter<DependencyP, Edges>::type;

  public:
    struct type { // mult value return
      using Node = N;
      using Descendants = typename Map<GetIn, NodeDescendants>::type;
    };
  };

  // bind node descedants. List<{Node, Descendants}>
  using NodeDescendants = typename Map<GetNodeDescedants, Nodes>::type;

  // we need to make sure the graph is a DAG, so there should be a loop check.
  // sorting
  template <typename Node> class SortNodes {};
};

#define graph(...)                                                             \
  Graph<__VA_ARGS__> {}

#define node(node_) auto (*)(NodeWrapper<node_>)
#define link(link_) link_->void

void Parsec() { puts("parsec"); }
void Ast() { puts("Ast.hs"); }
void Parser() { puts("Parser.hs"); }
void LLVMhs() { puts("llvm-hs"); }
void Codegen() { puts("Codegen.hs"); }
void Object() { puts("object"); }
