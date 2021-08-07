#include <iostream>
#include <memory>
#include <vector>

// ok let's make it simple. what 's the goal eventually?
// 1. something like this, but different visitors give you different way to
// traverse. (render_html/codegen/... whatever)
// 2. A node is a product type
// 3. visit funcition can return different types
// 4. if we want a tree accept different visitors the accept function need to
//    be somehow polymorphic

// Concrete tree with no polymorphic visitor what so ever.
struct Node {
  virtual ~Node() = default;
  virtual std::string render_html() const = 0;
};

struct Text final : public Node {
  std::string content_;
  Text(std::string content) : content_(content) {}
  std::string render_html() const override { return content_; }
};

struct Doc final : public Node {
  std::vector<std::unique_ptr<Node>> children_;

  Doc(std::vector<std::unique_ptr<Node>> &&children)
      : children_(std::move(children)) {}

  std::string render_html() const override {
    std::string result = "<head></head>\n<body>\n";
    for (auto &child : children_) {
      result += child->render_html();
    }
    result += "</body>";
    return result;
  }
};

// PS marking leaf nodes final to avoid slicing issues.
int main(void) {
  std::string result = "";

  // so annoying.
  std::vector<std::unique_ptr<Node>> v;
  v.emplace_back(std::make_unique<Text>(Text("<p>")));
  v.emplace_back(std::make_unique<Text>(Text("this is a paragraph")));
  v.emplace_back(std::make_unique<Text>(Text("</p>")));

  auto t = Text("asd");
  result = t.render_html();
  std::cout << "text: " << result << std::endl;

  auto tree = Doc(std::move(v));
  result = tree.render_html();
  std::cout << "doc: " << result << std::endl;
  return 0;
}
