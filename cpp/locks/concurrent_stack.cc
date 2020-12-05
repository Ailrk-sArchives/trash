#include <atomic>
#include <iostream>
#include <memory>
#include <random>
#include <thread>

template <typename T> class CouncurrentStack {
  // declare a private Ndoe type with shared_ptr to the next node.
  struct Node {
    T t;
    std::shared_ptr<Node> next;
  };

  // the head of the stack.
  // Because we are using shared ptr, it's not thread safe
  // when you tries to access resouces.
  // atomic operations need to be used to make it thread safe.
  std::shared_ptr<Node> head;

  // delete copy constructor.
  CouncurrentStack(const CouncurrentStack &) = delete;
  void operator=(const CouncurrentStack &) = delete;

public:
  CouncurrentStack() = default;
  ~CouncurrentStack() = default;

  // Create a reference type help user access the
  // shared pointer easier.
  // These class owns the shared pointer,
  // and only expose access.
  class reference {
    std::shared_ptr<Node> p;

  public:
    reference(std::shared_ptr<Node> p) : p(p) {}
    T &operator*() { return p->t; }
    T *operator->() { return &p->t; }
  };

  // find the first occurrence in the stack.
  auto find(T t) const {
    // head needs to be loaded atomically
    // if not, if the stack is modified while it's loading
    // the head might not be the same.
    auto p = std::atomic_load(&head);
    while (p && p->t != t) {
      p = p->next;
    }
    // move the shared pointer into the
    // reference.
    return reference(std::move(p));
  }

  // if it doens't load atomically the head
  // get loaded might not be the same.
  auto front() const { return reference(std::atomic_load(&head)); }

  // TODO
  void push_front(T t) {
    auto p = std::make_shared<Node>();
    p->t = t;
    p->next = std::atomic_load(&head);
    // works like a spinlock.
    while (!std::atomic_compare_exchange_weak(&head, &p->next, p)) {
    }
  }

  void pop_front() {
    auto p = head.load();
    while (!std::atomic_compare_exchange_weak(&head, &p, p->next)) {
    }
  }
};

int main(void) {

  CouncurrentStack<int> stack;

  for (int i = 0; i < 4; ++i) {
    stack.push_front(i);
  }

  // for (int i = 0; i < 1; ++i) {
  // }

  std::thread([&stack]() {
    stack.push_front(9);
    std::cout << *stack.front() << std::endl;
    std::cout << "good" << std::endl;
  }).detach();

  std::cout << *stack.find(9) << std::endl;

  return 0;
}
