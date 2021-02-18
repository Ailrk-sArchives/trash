#include <iostream>
#include <memory>
#include <vector>

// You cannot initialize vector with unique_ptr with initializer list.
// The reason is the vector's construct will build a initalizer list first
// which will copy unique_ptrs.
template <typename T, typename... Args>
std::vector<std::unique_ptr<T>> make_uniquevec(const Args... args) {
  std::vector<std::unique_ptr<T>> v;

  // replicate statement, ooh..
  (v.push_back(std::make_unique<T>(args)), ...);

  return v;
}

int main(int argc, char *argv[]) {

  auto vec = make_uniquevec<int>(1, 2, 3, 4, 5);

  for (auto &ptr : vec) {
    std::cout << *ptr << " ";
  }
  std::cout << std::endl;

  auto _ = std::move(vec[0]);
  std::cout << (vec[0] == nullptr) << std::endl;

  return 0;
}
