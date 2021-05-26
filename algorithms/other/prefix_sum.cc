#include <iostream>
#include <string_view>
#include <vector>
#include <numeric>
#include <type_traits>

// prefix sum on n dimensional array.

// given matrix:
// 1 2 4 3
// 5 1 2 4
// 6 3 5 9

// The prefix sum is:
// 1  3  7  10
// 6  9  15 22
// 12 18 29 45


template <typename T>
void print_vec(std::string_view msg, const std::vector<T> &vec) {
  std::cout << msg;
  for (auto & v: vec) {
    std::cout << v << " ";
  }
  std::cout << std::endl;
}

// template prefix sum.
template <typename InIter, typename OutIter>
OutIter prefix_sum(InIter first, InIter last, OutIter out) {
  if (first == last) return out;

  auto sum = *first;
  *out = sum;

  while (++first != last) {
    sum = std::move(sum) + *first;
    *++out = sum;
  }

  return ++out;
}

// principle of inclusion/exclusion.
// bascially:
// |A υ B υ C| = |A| + |B| + |C| - |A ∩ B| - |B ∩ C| - |C ∩ A| + |A ∩ B ∩ C|

int main(void)
{
  std::vector<int> vec {5, 3, 4, 8, 10, 2, 19, 7};
  std::vector<int> out {};

  std::partial_sum(std::begin(vec), std::end(vec), std::back_inserter(out));
  print_vec("stl partial sum: ", out);

  out = {};
  prefix_sum(std::begin(vec), std::end(vec), std::back_inserter(out));
  print_vec("prefix sum: ", out);


  return 0;
}
