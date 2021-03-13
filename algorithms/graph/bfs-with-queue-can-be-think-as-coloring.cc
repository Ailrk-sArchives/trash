// A different way of thinking about breath first search.
// 1. white: default for all nodes at the beginning
// 2. black: nodes that are not connected with white
// 3. gray: the remainnig (connected both black and white)
//
// And the stack is just
//
//      b
//     / \
//    g   g
//   / \   \
//   w w    w

#include <deque>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>

enum class Color { White = 1, Gray, Black };

template <typename T> struct Node {
  using type = T;

  T value;
  Color color = Color::White;
  int distance = 0;

  // the map owns the node.
  std::optional<std::reference_wrapper<Node>> pred = std::nullopt;
};

template <typename T> struct NodeHash {
  std::size_t operator()(const Node<T> &n) const noexcept {
    size_t h1 = static_cast<int>(n.color);
    size_t h2 = std::hash<int>()(n.distance) << h1;
    size_t h3 = std::hash<size_t>()(static_cast<size_t>(&n.value));
    return h2 ^ (h3 << 1);
  }
};

template <typename T> Node(T, auto...) -> Node<T>;

template <typename T> struct WeightedNode : Node<T> { int weight; };

template <typename T, typename N = Node<T>>
using Graph =
    std::unordered_map<N, std::vector<std::unique_ptr<N>>, NodeHash<T>>;

// takes a graph and a source s
template <typename T, typename N = Node<T>> void bfs(const Graph<N> &g, N &s) {

  // initalize nodes.
  for (auto &[k, v] : g) {
    g[k].color = Color::White;
    g[k].distance = static_cast<int>(-1);
    g[k].pred = std::nullopt;
  }

  // root node.
  s.color = Color::Gray;
  s.distance = 0;
  s.pred = std::nullopt;

  std::deque<Node<T> &> queue;

  queue.push_back(s);
  while (queue.size() != 0) {
    Node<T> &v = queue.pop_front();

    for (auto &n : g[v]) {
      if (n.color == Color::White) {
        n.color = Color::Gray;
        n.distance++;
        n.pred = v;
        queue.push_back(n);
      }
    }
    v.color = Color::Black;
  }
}

int main(void) {
  Graph<int> graph = {Node{1}, {Node{2}}};
  return 0;
}
