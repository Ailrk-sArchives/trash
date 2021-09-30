#include <iostream>
#include <memory>
#include <vector>

// compact NodeLike and NodeValue together.

struct Node {

    struct Base {
        virtual ~Base() = default;
        virtual std::string render_html() const = 0;
        virtual std::unique_ptr<Base> clone() const = 0;
    };

    // a generic wrapper for concrete implementation.
    template <typename T> struct Wrapper final : public Base {
        T obj_;
        Wrapper(T obj)
            : obj_(std::move(obj)) {
            static_assert(std::is_copy_constructible_v<T>,
                          "Node is not copy constructable");
        }
        std::unique_ptr<Base> clone() const override {
            return std::make_unique<Wrapper<T>>(obj_);
        }
        std::string render_html() const override { return obj_.render_html(); }
    };

    std::unique_ptr<Base> ptr_;

    template <typename T>
    Node(T obj)
        : ptr_(std::make_unique<Wrapper<T>>(std::move(obj))) {}
    Node(const Node &other)
        : ptr_(other.ptr_->clone()) {}

    Node &operator=(const Node &other) {
        ptr_ = other.ptr_->clone();
        return *this;
    }

    std::string render_html() const { return ptr_->render_html(); }
};

struct Text {
    std::string content_;
    Text(std::string content)
        : content_(content) {}

    std::string render_html() const { return content_; }
};

struct Doc {
    std::vector<Node> children_;
    Doc(std::vector<Node> &&children)
        : children_(std::move(children)) {}
    std::string render_html() const {
        std::string result = "<head>...</head><body>";
        for (auto &n : children_) {
            result += n.render_html();
        }
        result += "</body>";
        return result;
    }
};

// take different node types.
void print_render(const Node &node) {
    std::cout << node.render_html() << std::endl;
}

// static_assert(std::is_base_of_v<Node, Text>);

int main(void) {
    {
        auto t1 = Text("as");
        print_render(t1);
    }

    {
        std::vector<Node> v{ Text("<p>"),
                             Text("some string"),
                             Text("Some more string"),
                             Text("Really useless string"),
                             Text("Red pig"),
                             Text("</p>") };

        for (auto &n : v) {
            print_render(n);
        }

        auto d = Doc(std::move(v));
        print_render(d);
    }

    return 0;
}
