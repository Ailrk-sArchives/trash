#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <string>
#include <type_traits>
#include <vector>

// accumulate
template <typename InputIt, typename T>
constexpr T my_accumulate(InputIt first, InputIt last, T init) {
  for (; first != last; ++first) {
    init = std::move(init) + *first;
  }
  return init;
}

template <typename InputIt, typename T, typename BinaryOperator>
constexpr T my_accumulate(InputIt first, InputIt last, T init,
                          BinaryOperator op) {
  for (; first != last; ++first) {
    init = op(std::move(init), *first);
  }
  return init;
}

// c++ folds use accumulate
// there is no difference between foldl' and foldr since
// everything is eager.
// default is foldl, and to foldr, just fold on the reverse
// iterator.
// There will be no performance panelty since it's just two
// pointers.
void folds() {
  std::cout << "folds" << std::endl;
  std::vector<int> vec{1, 2, 3};

  auto result =
      std::accumulate(vec.begin(), vec.end(), 1, std::multiplies<int>());
  std::cout << result << std::endl;

  // default use +
  auto sum = std::accumulate(vec.begin(), vec.end(), 0);

  auto dash_fold = [](std::string a, int b) {
    return std::move(a) + '-' + std::to_string(b);
  };

  // lfold
  std::string s = std::accumulate(std::next(vec.begin()), vec.end(),
                                  std::to_string(vec[0]), dash_fold);

  // rfold
  std::string rs = std::accumulate(std::next(vec.rbegin()), vec.rend(),
                                   std::to_string(vec.back()), dash_fold);

  std::cout << s << std::endl;
  std::cout << rs << std::endl;
}

// partial sum
template <typename InputIt, typename OutputIt>
constexpr OutputIt my_partial_sum(InputIt first, InputIt last,
                                  OutputIt d_first) {
  if (first == last)
    return d_first;

  typename std::iterator_traits<InputIt>::value_type sum = *first;
  *d_first = sum;

  while (++first != last) {
    sum = std::move(sum) + *first;
    *++d_first = sum;
  }
  return ++d_first;
}

template <typename InputIt, typename OutputIt, typename BinaryOperator>
constexpr OutputIt my_partial_sum(InputIt first, InputIt last, OutputIt d_first,
                                  BinaryOperator op) {
  if (first == last) return d_first;

  typename std::iterator_traits<InputIt>::value_type sum = *first;
  *d_first = sum;

  while (++first != last) {
    sum = op(std::move(sum), *first);
    *++d_first = sum;
  }

  return ++d_first;
}

void partial_sum() {
  std::cout << "partial_sum" << std::endl;
  std::vector<int> vec{2, 2, 2, 2};
  std::partial_sum(vec.begin(), vec.end(), vec.begin());
  for (auto v : vec) {
    std::cout << v << std::endl;
  }

}

int main(void) {
  folds();
  partial_sum();
  return 0;
}
