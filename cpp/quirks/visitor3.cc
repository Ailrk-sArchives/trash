#include <iostream>
#include <memory>
#include <vector>

// fully erase the type. not even the node.
struct Node {
  virtual ~Node() = default;
  virtual std::string render_html() const = 0;
  virtual std::unique_ptr<Node> clone() const = 0;
};

///////////////////////////////////////////////////////////////////////////////
// duck typing
// we erase the type T and turn it into NodeLike.
template <typename T> struct NodeLike final : public Node {
  T obj_;
  NodeLike(T obj) : obj_(std::move(obj)) {
    static_assert(std::is_copy_constructible_v<T>,
                  "Node is not copy constructable");
  }

  std::unique_ptr<Node> clone() const override {
    return std::make_unique<NodeLike<T>>(obj_);
  }

  std::string render_html() const override { return obj_.render_html(); }
};

// Get value semantics wrapper
// again a exitential type to erase node type.
struct NodeValue {
  std::unique_ptr<Node> ptr_;

  // convert T to NodeLike to duck type it.
  template <typename T>
  NodeValue(T obj) : ptr_(std::make_unique<NodeLike<T>>(std::move(obj))) {}
  NodeValue(const NodeValue &other) : ptr_(other.ptr_->clone()) {}
  NodeValue &operator=(const NodeValue &other) {
    ptr_ = other.ptr_->clone();
    return *this;
  }

  Node *operator->() const { return ptr_.get(); }
  Node &operator*() const { return *ptr_; }
};

///////////////////////////////////////////////////////////////////////////////
struct Text {
  std::string content_;
  Text(std::string content) : content_(content) {}

  std::string render_html() const { return content_; }
};

struct Doc {
  std::vector<NodeValue> children_;
  Doc(std::vector<NodeValue> &&children) : children_(std::move(children)) {}
  std::string render_html() const {
    std::string result = "<head>...</head><body>";
    for (auto &n : children_) {
      result += n->render_html();
    }
    result += "</body>";
    return result;
  }
};

static_assert(!std::is_default_constructible_v<NodeValue>);
static_assert(std::is_copy_constructible_v<NodeValue>);
static_assert(std::is_move_constructible_v<std::vector<NodeValue>>);
static_assert(!std::is_trivially_move_constructible_v<std::vector<NodeValue>>);

template <typename T> NodeValue make_nodevalue(T obj) {
  return NodeValue(std::move(obj));
}

// ducktying + value semantics
int main(void) {
  {
    auto t1 = make_nodevalue(Text("Good"));
    std::cout << t1->render_html() << std::endl;
  }

  {

    std::vector<NodeValue> v{};
    v.emplace_back(make_nodevalue(Text("<p>")));
    v.emplace_back(make_nodevalue(Text("Some other things")));
    v.emplace_back(make_nodevalue(Text("</p>")));
    auto tree = make_nodevalue(Doc(std::move(v)));
    std::cout << tree->render_html() << std::endl;
  }

  return 0;
}
