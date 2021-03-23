#include <iostream>
#include <memory>

struct pod_t {
  int a;
  int b;
  int c;
};

int main(void) {

  pod_t *on_heap = new pod_t{1, 2, 3};
  std::cout << "on_heap: " << on_heap << std::endl;
  std::cout << "on_heap: " << on_heap->a << std::endl;

  auto a = std::unique_ptr<pod_t>(new pod_t());
  std::cout << a->a << std::endl;
  std::cout << a->b << std::endl;
  std::cout << a->c << std::endl;

  auto b = std::make_unique<pod_t>(1, 2, 3);
  std::cout << b->a << std::endl;
  std::cout << b->b << std::endl;
  std::cout << b->c << std::endl;
  return 0;
}
