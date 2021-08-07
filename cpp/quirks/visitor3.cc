#include <iostream>
#include <memory>
#include <vector>

// fully erase the type. not even the node.

struct Node {
  virtual ~Node() = default;
  virtual std::string render_html() const = 0;
  virtual std::unique_ptr<Node> clone() const = 0;
};

// a node like wrapper that wraps anything has render_html function.
template <typename T> class NodeLike final : public Node {
  T obj_;
  NodeLike(T obj) : obj_(std::move(obj)) {}

  std::unique_ptr<Node> clone() const override {
    return std::make_unique<NodeLike<T>>(obj_);
  }

  std::string render_html() const override { return obj_.render_html; }
};
