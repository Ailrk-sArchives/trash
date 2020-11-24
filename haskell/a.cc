#include <iostream>
#include <memory>

void inc(int *v) { *v += 1; }

int main(int argc, char *argv[]) {
  auto ptr = std::make_unique<int>(10);
  inc(ptr.get());

  return 0;
}
