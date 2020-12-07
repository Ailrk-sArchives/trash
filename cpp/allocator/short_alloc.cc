// https://howardhinnant.github.io/stack_alloc.html

#ifndef SHORT_ALLOC_
#define SHORT_ALLOC_

#include <cassert>
#include <cstddef>
#include <cstdint>

template <size_t N, size_t Alignment = alignof(std::max_align_t)> class Arena {
  alignas(Alignment) char buf[N];

  char *ptr;

public:
  ~Arena() { ptr = nullptr; }

  // Intialize the ptr to the buf position.
  Arena() noexcept : ptr(buf) {}

  // Of course, we don't want our Arena be copied.
  Arena(const Arena &) = delete;
  Arena &operator=(const Arena &) = delete;

  template <size_t ReqAlign> char *allocate(size_t n);
  void deallocate(char *p, size_t n) noexcept;

  static constexpr size_t size() noexcept { return N; }

  size_t used() const noexcept { return static_cast<size_t>(ptr - buf); }
  void reset() noexcept { ptr = buf; }

private:
  static size_t align_up(size_t n) noexcept {
    return (n + (Alignment - 1)) & ~(Alignment - 1);
  }

  bool pointer_in_buffer(char *p) noexcept {
    return std::uintptr_t(buf) <= std::uintptr_t(p) &&
           std::uintptr_t(p) <= std::uintptr_t(buf) + N;
  }
};

template <size_t N, size_t Alignment>
template <size_t ReqAlign>
char *Arena<N, Alignment>::allocate(size_t n) {
  static_assert(ReqAlign <= Alignment, "alignment is too small");
  assert(pointer_in_buffer(ptr) && "short_alloc has outlived arena");
  auto const aligned_n = align_up(n);
  if (static_cast<decltype(aligned_n)>(buf + N - ptr) >= aligned_n) {
    char* r = ptr;
    ptr += aligned_n;
    return r;
  }

  static_assert(Alignment <= alignof(std::max_align_t), "you've chosen an "
                "alignment that is larger than alignof(std::max_align_t), and "
                "cannot be guaranteed by normal operator new");

  return static_cast<char*>(::operator new(n));
}

template <size_t N, size_t Alignment>
void Arena<N, Alignment>::deallocate(char *p, size_t n) noexcept {

  assert(pointer_in_buffer(ptr) && "shrot_alloc has outlived arena");
  if (pointer_in_buffer(p)) {
    n = align_up(n);
    if (p + n == ptr) {
      ptr = p;
    }
  } else {
    ::operator delete(p);
  }
}

#endif /* ifndef SHORT_ALLOC_ */
