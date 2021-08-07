#include <iostream>
#include <memory>
#include <vector>

// from visitor 1 we can make a new version with value semantics.
// ditch unique_ptr.
// vector, string are all this type of value.

struct Node {
  virtual ~Node() = default;
  virtual std::string render_html() const = 0;
  virtual std::unique_ptr<Node> clone() const = 0;
};

// value semantics wrapper wrap Nodes into value semantic.
// value semantics means we want to be able to copy stuffs. To copy a node
// we copy the content of the node and create a new unique ptr to manage it.
struct NodeValue {
  std::unique_ptr<Node> ptr_;

  template <std::derived_from<Node> T>
  NodeValue(T obj) : ptr_(std::make_unique<T>(std::move(obj))) {}

  NodeValue(const NodeValue &other) : ptr_(other->clone()) {}

  Node *operator->() const { return ptr_.get(); }
  Node &operator*() const { return *ptr_; }
};

// we mark it final so all virutal funcition are implemented
struct Text final : public Node {
  std::string content_;
  Text(std::string content) : content_(content) {}

  std::string render_html() const override { return content_; }
  std::unique_ptr<Node> clone() const override {
    return std::make_unique<Text>(content_);
  }
};

struct Doc final : public Node {
  std::vector<NodeValue> children_;
  Doc(const std::vector<NodeValue> &&children) : children_(std::move(children)) {}

  std::unique_ptr<Node> clone() const override {
    std::vector<std::unique_ptr<Node>> children;
    for (auto &c : children_) {
      children.push_back(c->clone());
    }
    return std::make_unique<Doc>(std::move(children_));
  }

  std::string render_html() const override {
    std::string result = "<head></head>\n<body>\n";
    for (auto &child : children_) {
      result += child->render_html();
    }
    result += "</body>";
    return result;
  }
};

template <std::derived_from<Node> T> NodeValue make_node(T &&node) {
  return NodeValue(std::move(node));
}

int main(void) {
  std::vector<NodeValue> v;
  v.emplace_back(make_node(Text("<p>")));
  v.emplace_back(make_node(Text("this is a paragraph")));
  v.emplace_back(make_node(Text("</p>")));

  auto doc = Doc(std::move(v));
  auto tree = make_node(std::move(doc));

  auto result = tree->render_html();
  std::cout << result << std::endl;

  return 0;
}

// What we did?
// 1. Created a exitential type NodeValue that hides polymorphic pointers points
//    to Node.
// 2. Give NodeValue value semantics by implementing clone.
// 3. Wrapping Nodes into NodeValue.

// What we get?
// 1. Value semantics of nodes
// 2. unified interface.

// What we want?
// 1. implementation of different functionalities are scattered in different
//    class. Hard to extend.
// 2.
