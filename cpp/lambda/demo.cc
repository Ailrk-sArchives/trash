#include <algorithm>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <vector>

class Base {
public:
  int a;
  int b;

  Base(int a, int b) : a(a), b(b) {}

  void scall() { std::cout << "I'm base" << std::endl; }
  virtual void vcall() { std::cout << "Virtual: I'm base" << std::endl; }
};

class Dervied : public Base {

public:
  int c;
  Dervied(int a, int b, int c) : c(c), Base(a, b) {}

  void scall() { std::cout << "I'm derived" << std::endl; }
  void vcall() override { std::cout << "Virtual: I'm derived" << std::endl; }
};

int main(void) {

  { // a templateed lambda
    auto tlam = []<typename T>(T a, auto &&b) { return a < b; };
    int val = 20;
    auto r = tlam(10, val); // lval 20 is converted to rval. move is redundant.
    std::cout << "> " << r << std::endl;
  }

  { // move lambda
    // note you need to construct a initializer list here.
    // C++ can't infer the type (braced list is not deducible ...).
    std::cout << "> ";
    auto pnums = std::make_unique<std::vector<int>>(
        std::initializer_list<int>({1, 2, 3}));
::
    for (const auto &v : *pnums) {
      std::cout << "before move: " << v << " ";
    }
    std::cout << std::endl;

    auto f = [ptr = std::move(pnums)]() noexcept {
      std::cout << "value moved from pnums: " << std::endl;
      for (const auto &v : *ptr) {
        std::cout << v << " " << std::endl;
      }
    };

    f();
    std::cout << "is pnum nullptr?: " << (pnums == nullptr) << std::endl;
  }

  { // make_unique allocate on heap.
    std::cout << "> ";
    std::string a("good");
    auto ptr =
        std::make_unique<std::string>(a); // this will create a new string.
    a.append("sd");

    std::cout << "make_unique makes another string. See:" << std::endl;
    std::cout << a << std::endl;
    std::cout << *ptr << std::endl;
  }

  { // transfer ownership between unique pointers.
    std::cout << "> ";

    auto ptr1 = std::make_unique<Base>(1, 2);
    auto ptr2 = std::move(ptr1);

    std::cout << "ptr1 is null? " << (ptr1 == nullptr) << std::endl;
    std::cout << "ptr2: " << (*ptr2).a << std::endl;
  }

  { // shared pointer.
    // an vector of shared pointers.
    std::cout << "> " << std::endl;
    std::vector<std::shared_ptr<Base>> v{
        std::make_shared<Base>(1, 2), std::make_shared<Base>(1, 3),
        std::make_shared<Base>(1, 4), std::make_shared<Base>(1, 8),
        std::make_shared<Base>(1, 9),
    };

    std::vector<std::shared_ptr<Base>> v2;

    // use shared pointer with stl algorithms.
    // your elements can have multiple references.
    // Here you basically copy the ptr to another array. It's not allowed
    // For unique pointers.
    std::remove_copy_if(v.begin(), v.end(), std::back_inserter(v2),
                        [](std::shared_ptr<Base> s) { return s->b >= 3; });

    std::cout << "v now: " << std::endl;
    for (const auto &v : v) {
      v->scall();
      std::cout << v << " ";
    }
    std::cout << std::endl;

    std::cout << "v2 now: " << std::endl;
    for (const auto &v : v2) {
      v->vcall();
      std::cout << v << " ";
    }
    std::cout << std::endl;
  }

  return 0;
}
