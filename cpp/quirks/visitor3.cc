#include <iostream>
#include <memory>
#include <vector>

// fully erase the type. not even the node.

struct Node {

  struct Base {
    virtual ~Base() = default;
    virtual std::string render_html() const = 0;
    virtual std::unique_ptr<Base> clone() const = 0;
  };

  // a generic wrapper for concrete implementation.
  template <typename T> struct Wrapper final : public Base {
    T obj_;
    Wrapper(T obj) : obj_(std::move(obj)) {}
    std::unique_ptr<Base> clone() const override {
      return std::make_unique<Wrapper<T>>(obj_);
    }
    std::string render_html() const override { return obj_.render_html(); }
  };

  std::unique_ptr<Base> ptr_;

  template <typename T>
  Node(T obj) : ptr_(std::make_unique<Wrapper<T>>(std::move(obj))) {}
  Node(const Node &other) : ptr_(other.ptr_->clone()) {}

  Node &operator=(const Node &other) {
    ptr_ = other.ptr_->clone();
    return *this;
  }

  std::string render_html() const { return ptr_->render_html(); }
};

///////////////////////////////////////////////////////////////////////////////
// a node like wrapper that wraps anything has render_html function.
template <typename T> struct NodeLike final : public Node::Base {
  T obj_;
  NodeLike(T obj) : obj_(std::move(obj)) {}

  std::unique_ptr<Node::Base> clone() const override {
    return std::make_unique<NodeLike<T>>(obj_);
  }

  std::string render_html() const override { return obj_.render_html; }
};

// again a exitential type to erase node type.
struct NodeValue {
  std::unique_ptr<Node> ptr_;

  template <typename T>
  NodeValue(T obj) : ptr_(std::make_unique<NodeLike<T>>(std::move(obj))) {}
};

///////////////////////////////////////////////////////////////////////////////
// Duck types.
struct Text {
  std::string content_;
  Text(std::string content) : content_(content) {}

  std::string render_html() const { return content_; }
};

struct Doc {
  std::vector<Node> children_;
  std::string render_html() const {
    std::string result = "<head>...</head><body>";
    for (auto &n : children_) {
      result += n.render_html();
    }
    result += "</body>";
    return result;
  }
};

int main(void)
{

  return 0;
}
