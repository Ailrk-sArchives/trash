#include <algorithm>
#include <array>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <memory>
#include <vector>

namespace allocator_demo::minialloc {

// after you implemented this parts you get a workable
// customized allocator.
template <typename T> struct MinimalAllocator {

  using value_type = T;

  template <typename U>
  MinimalAllocator(MinimalAllocator<U> const &) noexcept {}
  MinimalAllocator() = default;
  ~MinimalAllocator() = default;

  [[nodiscard]] T *allocate(size_t n);

  void deallocate(T *p, size_t);
};

template <typename T> T *MinimalAllocator<T>::allocate(size_t n) {
  std::cout << "allocating" << std::endl;
  return static_cast<T *>(::operator new(n * sizeof(T *)));
}

template <typename T> void MinimalAllocator<T>::deallocate(T *p, size_t n) {
  std::cout << "deallocating" << std::endl;
  ::operator delete(p);
}

template <typename T>
bool operator==(MinimalAllocator<T> const &, MinimalAllocator<T> const &) {
  return true;
};

template <typename T>
bool operator!=(MinimalAllocator<T> const &, MinimalAllocator<T> const &) {

  return false;
};
} // namespace allocator_demo::minialloc

namespace allocator_demo::myalloc {

template <typename T> struct MyAlloc {
  using value_type = T;

  MyAlloc() = default;
  template <typename U> constexpr MyAlloc(const MyAlloc<U> &) noexcept {}
  ~MyAlloc() = default;

  [[nodiscard]] T *allocate(size_t n) {
    if (n > std::numeric_limits<size_t>::max() / sizeof(T)) {
      throw std::bad_alloc();
    }

    if (auto p = static_cast<T *>(malloc(sizeof(T) * n))) {
      report(p, n);
      return p;
    }

    throw std::bad_alloc();
  }

  void deallocate(T *p, size_t n) noexcept {
    report(p, n, false);
    free(p);
  }

private:
  void report(T *p, size_t n, bool alloc = true) {
    std::cout << (alloc ? "Alloc" : "Dealloc") << ": " << sizeof(T) * n
              << " bytes at " << std::hex << std::showbase
              << reinterpret_cast<void *>(p) << std::dec << std::endl;
  }
};

} // namespace allocator_demo::myalloc

// simple arean
namespace allocator_demo::arena {

template <typename T> struct Arena {
  using value_type = T;
  static constexpr size_t N = 1024;

  Arena() : arena_{}, base_(arena_.begin()), current_(arena_.begin()) {}

  ~Arena() {
    std::cout << "see ya" << std::endl;
  }
  template <typename U> constexpr Arena(const Arena<U> &) {}

  [[nodiscard]] T *allocate(size_t n) {
    if (n > N / sizeof(T)) {
      throw std::bad_alloc();
    }

    current_ += n * sizeof(T);
    auto p = static_cast<T *>(reinterpret_cast<void *>(current_));
    report(p, n);

    return p;
  }

  void deallocate(T *, size_t n) {
    current_ -= n * sizeof(T);
    auto p = static_cast<T *>(reinterpret_cast<void *>(current_));
    report(p, n, false);
  }

private:
  std::array<char, N> arena_;
  char *base_;
  char *current_;

  void report(T *p, size_t n, bool alloc = true) const {
    auto distance = n * sizeof(T);
    std::cout << (alloc ? "Alloc" : "Dealloc") << ": " << n * sizeof(T)
              << " bytes at " << std::hex << std::showbase
              << reinterpret_cast<void *>(p) << std::dec << ", \n the arena is "
              << 100.0 * distance / N << " percent full" << std::endl;
  }
};

} // namespace allocator_demo::arena

#define CHECK(s, init)                                                         \
  {                                                                            \
    init v{};                                                                  \
    std::cout << s << std::endl;                                               \
    for (int i = 0; i < 50; ++i) {                                             \
      v.push_back(i);                                                          \
    }                                                                          \
    std::cout << "value[10]: " << v[10] << std::endl;                          \
    v.erase(std::remove_if(v.begin(), v.end(), [](int n) { return n > 18; }),  \
            v.end());                                                          \
  }

int main(void) {
  CHECK("v", std::vector<int>);

  using V1 = std::vector<int, allocator_demo::myalloc::MyAlloc<int>>;
  CHECK("myalloc", V1);

  using V2 = std::vector<int, allocator_demo::arena::Arena<int>>;
  CHECK("arena", V2);

  { std::vector<int, allocator_demo::arena::Arena<int>> v{1, 2, 3}; }

  return 0;
}
