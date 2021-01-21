#include <cassert>
#include <iostream>
#include <memory>
#include <optional>

// tail is persistent, so we don't need to make a copy.
template <typename T> class List {
  struct Element {
    Element(T v, const std::shared_ptr<Element> &tail) : val_(v), next_(tail) {}
    T val_;
    std::shared_ptr<Element> next_;
  };

  std::shared_ptr<Element> head_;

public:
  List(){};
  List(T val, const List &tail)
      : head_(std::make_shared<Element>(val, tail.head_)) {}
  explicit List(std::shared_ptr<Element> items) : head_(items) {}
  bool is_empty() const { return !head_; }

  std::optional<T> front() const {
    if (is_empty()) {
      return {};
    }
    return head_->val_;
  }

  std::optional<List> pop_front() const {
    if (is_empty()) {
      return {};
    }
    return List(head_->next_);
  }

  List push_front(T v) const { return List(v, *this); }
};

int main(void) { return 0; }
