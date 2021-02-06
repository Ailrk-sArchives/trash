#include <functional>
#include <iostream>

template <int Init = 0> class M {
private:
  int count = Init;

public:
  M() = default;
  M(int n) : count(n) {}

  M good() {
    count++;
    return *this;
  }

  M bad() {
    count++;
    return M(count);
  }

  M automorph(const std::function<int(int)> &f) { return M(f(count)); }

  void show() { std::cout << "count: " << count << std::endl; }
};

int main(void) {
  M<0> m;
  m.show();
  m.good().good().good().show();
  m.bad().bad().bad().show();

  auto f = [](int v) { return v + 1000; };
  M<0> m1;
  m1.good()
      .good()
      .automorph([](int v) { return v + 10; })
      .automorph([](int v) { return v + 100; })
      .automorph(f)
      .automorph(f)
      .show();

  return 0;
}
