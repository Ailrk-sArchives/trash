#include <functional>
#include <iostream>

template <typename T> struct Thing {
  T t;
  using Type = T;
  template <typename U> Thing(U u) {}
};

Thing(const char *)->Thing<std::string>;

template <typename Queue, typename F> class Tasks {
  Tasks(Queue &&q, const F &on_finished);
};

template <typename Queue, typename F>
Tasks(Queue, F) -> Tasks<Queue, std::function<void()>>;


int main(void) {
  Thing thing{"asd"};
  using p = decltype(thing)::Type;
  p str{"god"};
  return 0;
}
