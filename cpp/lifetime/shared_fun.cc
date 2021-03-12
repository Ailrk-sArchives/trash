#include <functional>
#include <iostream>
#include <memory>

// original move semantics proposal.
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2002/n1377.htm

class Caller {

public:
  std::shared_ptr<std::function<int(int)>> func;

  Caller(const std::function<int(int)> &fn)
      : func(std::make_shared<std::function<int(int)>>(fn)) {}

  Caller(std::function<int(int)> &&fn)
      : func(std::make_shared<std::function<int(int)>>(std::move(fn))) {}
};

void test_by_rval() {
  std::cout << "\n";
  std::cout << "test rva" << std::endl;
  Caller c1 = Caller([](int a) -> int { return 99; });

  Caller c2 = c1;

  std::cout << (*c2.func)(1) << std::endl;
  std::cout << "c1 c2 count of func: " << c2.func.use_count() << std::endl;

  {
    Caller c3 = c2;
    std::cout << "create c3, count of func: " << c2.func.use_count()
              << std::endl;
  }
  std::cout << "c3 destoryed, count of func: " << c2.func.use_count()
            << std::endl;
}

void test_by_lval() {
  std::cout << "\n";
  std::cout << "test lval" << std::endl;

  auto fn = [](int a) -> int { return -100; };

  {
    Caller c1 = Caller(std::move(fn));
    std::cout << fn(1) << std::endl;
    Caller c2 = c1;
    std::cout << (*c2.func)(1) << std::endl;
    std::cout << "c1 c2 count of func: " << c2.func.use_count() << std::endl;
  }

  // it's still here. the reason is move just cast type.
  // use move semantics means you don't case what's the state of the source.
  std::cout << fn(1) << std::endl;
}

void test_by_delete() {
  std::cout << "\n";
  std::cout << "test rva" << std::endl;
  Caller c1 = Caller([](int a) -> int { return 99; });

  Caller c2 = c1;
  Caller c3 = c1;

  std::cout << (*c2.func)(1) << std::endl;
  std::cout << "c1 c2 c3 count of func: " << c2.func.use_count() << std::endl;

  c2.~Caller();
  c1.~Caller();
  std::cout << "c1 delelted, count of func: " << c2.func.use_count()
            << std::endl;
}

int main(void) {
  test_by_rval();
  test_by_lval();
  test_by_delete();
  return 0;
}
