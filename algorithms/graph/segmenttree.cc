#include <array>
#include <iostream>

template <typename T, size_t Size> class SegTree {
private:
  std::array<T, Size> data;

public:
  SegTree() {}
};
