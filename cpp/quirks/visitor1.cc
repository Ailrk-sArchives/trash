#include <iostream>
#include <memory>
#include <vector>

// Want a visitor, but really simpler way is to write differnet visit function
// direclty in the class.

// Ideally you want a type Visitor,
// which is accepted by Visitable::accept(Visitor&). Visitor can be polymorphic
// and Visitable can be polymorphic.
//
// Well that's kind hard to achieve with monomorphization and dynamic dispatch
// alone.
//
// Better way is just add function to each node and conform everything to the
// same abstract class.

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
