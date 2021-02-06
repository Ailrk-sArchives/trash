// Polymorphic memory resource.
// - Provide runtime polmorphism with single type argument to containers.
// - Client allocators store a pointer to a base class memory resource
// - No lateral propagation. An allcator sticks for life.

#include <array>
#include <chrono>
#include <list>
#include <memory_resource>

// simply allocate list on the stack.
// instead of allocating each nodes separately, we allocate the
// entire space first, then put item in it diresclty.
void lis() {
  std::array<char, 64> buffer{};
  std::fill_n(buffer.begin(), std::size(buffer) - 1, '_');
  std::pmr::monotonic_buffer_resource rsc{buffer.data(), buffer.size()};

  std::pmr::list<char> ls{{'a', 'b', 'c', 'd', 'e'}, &rsc};

  for (char ch = 'a'; ch <= 'z'; ++ch) {
    ls.push_back(ch);
  }
}
