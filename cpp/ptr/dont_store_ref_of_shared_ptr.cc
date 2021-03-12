#include <assert.h>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

struct Foo {
  int *data;

  Foo() {
    data = new int[10];
    for (int i = 0; i < 10; ++i) {
      data[i] = i;
    }
  }

  void shift1() {
    int tmp = data[9];
    for (int i = 9; i > 0; --i) {
      data[i] = data[i - 1];
    }
    data[0] = tmp;
  }

  std::string to_string() {
    std::stringstream ss;
    ss << "addr of data:  " << &data << " ";
    for (int i = 0; i < 10; ++i) {
      ss << data[i] << ", ";
    }
    return ss.str();
  }
};

static_assert(std::is_destructible_v<Foo>);

// this is good. use shared_ptr to manage resource with multiple owners.
using SF = std::shared_ptr<Foo>;

// this is bad, because you don't know if the underlying shared_ptr is
// valid.
using RSF = std::reference_wrapper<std::shared_ptr<Foo>>;

int main(void) {

  Foo f1;

  // Topic 1. is shared_ptr on stack or on heap?
  // this construct a new element that's different from f1.
  SF p0 = std::make_shared<Foo>(f1);

  // verify sure p1 is on the heap.
  std::cout << "stack address f1: " << &f1 << std::endl;

  // notice the value shared_ptr hold now is on the heap.
  // *p0 is
  std::cout << "stack address p0: " << &(*p0) << std::endl;
  std::cout << "stack address p0.get: " << p0.get() << std::endl;
  assert(&f1 != &(*p0));

  // change value through p0 should not affect f1.

  f1.shift1();
  std::cout << "*p0 after modify p0: " << p0->to_string() << std::endl;
  std::cout << "f1 after modify p0: " << f1.to_string() << std::endl;
  // cleary these two are allocated separately

  // Note: don't use shared ptr over stack allocated obj. doesn't make sense.

  // allocate on the heap.
  SF p1 = std::make_shared<Foo>();
  SF p2 = std::make_shared<Foo>();
  SF p3 = std::make_shared<Foo>();

  // this is quite problematic. if shared ptr is invalid you get a
  // null reference.
  RSF r1 = std::ref(p1);

  p1.reset();

  std::cout << "null referece ?" << r1.get() << std::endl;

  return 0;
}
