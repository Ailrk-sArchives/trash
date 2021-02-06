#include <iostream>
#include <string>
#include <vector>

struct Base {
  virtual std::string get_name() const = 0;
};

struct Bar : Base {
  std::string get_name() const override { return "bar"; }
};

struct Foo : Base {
  std::string get_name() const override { return "Fool"; }
};

// liskov substitution principle
// basically just have a polymorphic pointer which can behave as
// any of the subclass.
void print_name(std::vector<const Base *> vec) {
  for (auto v : vec)
    std::cout << v->get_name() << std::endl;
}

int main(void) {

  Foo foo;
  Bar bar;

  std::vector<const Base *> vec{&foo, &bar};

  print_name(vec);

  return 0;
}
