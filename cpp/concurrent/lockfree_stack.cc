// https://en.cppreference.com/w/cpp/atomic/atomic_compare_exchange
// https://stackoverflow.com/questions/40223599/what-is-the-difference-between-stdshared-ptr-and-stdexperimentalatomic-sha
// https://stackoverflow.com/questions/25199838/understanding-stdatomiccompare-exchange-weak-in-c11


// atomic_compare_exchange is the atomic swap.
// it's probably one of the most important operation for lock
// free data algorithms.

#include <atomic>
#include <iostream>
#include <memory>
#include <random>
#include <thread>

template <typename T> class LockFreeStack {
  struct node {
    std::shared_ptr<T> data;
    std::shared_ptr<node> next;
    node(const T &data) : data(std::make_shared<T>(data)) {}
  };

  std::shared_ptr<node> head;

public:
  void push(const T &data) {
    const std::shared_ptr<node> new_node = std::make_shared<node>(data);

    // put the current value of head into new_node->next
    new_node->next = std::atomic_load(&head);

    // make new_node the new head.
    // if the head is no longer what's stored in
    // new_node->next, that means some other thread must
    // have inserted a node just now.
    // So we should put that new head into new_node->next
    // and try again.
    while (!std::atomic_compare_exchange_weak(&head, &new_node->next, new_node))
      ;
  }

  std::shared_ptr<T> pop() {
    std::shared_ptr<node> old_head = std::atomic_load(&head);
    while (old_head &&
           !std::atomic_compare_exchange_weak(&head, &old_head, old_head->next))
      ;

    return old_head ? old_head->data : std::shared_ptr<T>();
  }
};

int main(void) {
  LockFreeStack<int> ls;

  ls.push(1);
  ls.push(2);

  auto out = ls.pop();
  auto out1 = out;

  std::cout << "value: " << (*out) << ", count: " << out.use_count() << std::endl;

  return 0;
}
