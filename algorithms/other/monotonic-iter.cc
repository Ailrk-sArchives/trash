#include <assert.h>
#include <deque>
#include <functional>
#include <iostream>
#include <iterator>
#include <type_traits>
#include <vector>

// Monotonic queue for sliding window problems.
// find the biggest value in a subsequence
//
// O(n) for querying. It's better then ST table and
// segment tree.
//
// The idea is to maintain a deque, and only include elements that are
// `possible` to be the biggest (smallest) value in the subsequence.

template <typename Iter, typename Comp> class monotonic_queue_iterator {
private:
  using T = typename std::iterator_traits<Iter>::value_type;
  std::deque<T> queue;
  size_t win_size;
  Iter first, last;

  Comp comp;

public:
  using difference_type = void;
  using value_type = std::deque<T>;
  using pointer = std::deque<T> *;
  using reference = std::deque<T> &;
  using const_reference = const std::deque<T> &;
  using iterator_category = std::input_iterator_tag;

  // initialize the first window.
  explicit constexpr monotonic_queue_iterator(Iter first, Iter last,
                                              size_t window_size, Comp comp)
      : first(first), last(last), win_size(window_size), comp(comp) {
    auto it = first;
    auto max_it = it;

    for (size_t i = 0; i < win_size; ++i) {
      std::cout << *it << std::endl;
      if (comp(*it, *max_it)) {
        max_it = it;
      }

      if (i < win_size - 1)
        ++it;
    }

    queue.push_back(*max_it);

    if (max_it != it) {
      queue.push_back(*it);
    }
  }

  explicit constexpr monotonic_queue_iterator(Iter first, Iter last,
                                              size_t window_size)
      : monotonic_queue_iterator(
            first, last, window_size,
            [](const auto &a, const auto &b) { return a > b; }) {}

  template <typename C>
  explicit constexpr monotonic_queue_iterator(const C &c, size_t window_size)
      : monotonic_queue_iterator(c.begin(), c.end(), window_size) {}

  constexpr friend bool operator==(const monotonic_queue_iterator &self,
                                   const monotonic_queue_iterator &other) {
    return self.win_size == other.win_size && self.queue == other.queue;
  }

  constexpr bool operator!=(const monotonic_queue_iterator &other) {
    return !operator==(other);
  }

  constexpr monotonic_queue_iterator<Iter, Comp> operator++() {
    auto it = ++first;
    std::advance(it, win_size - 1);

    auto qit = queue.rbegin();
    while (!(queue.size() == 0) && qit != queue.rend() && *qit < *it) {
      qit++;
    }

    queue.erase(qit.base(), queue.end());
    queue.push_back(*it);

    return *this;
  }

  constexpr monotonic_queue_iterator<Iter, Comp> operator++(int) {
    monotonic_queue_iterator tmp(*this);
    ++this;
    return tmp;
  }

  constexpr const_reference operator*() const { return queue; }
  constexpr const_reference operator*() { return queue; }

  constexpr pointer operator->() { return std::addressof(operator*()); }
};

// template <typename C>
// monotonic_queue_iterator(C c, size_t window_size)
//     -> monotonic_queue_iterator<typename C::iterator>;

int main(void) {
  std::vector<int> vec{1, 3, 6, 2, 5, 1, 7};
  monotonic_queue_iterator<std::vector<int>::iterator,
                           std::function<bool(int, int)>>
      mit(vec.begin(), vec.end(), 4);
  // monotonic_queue_iterator mit1(vec, 4);

  for (int i = 0; i < vec.size() - 4; ++i, ++mit) {
    auto q = *mit;
    for (auto &v : q) {
      std::cout << v << ", ";
    }
    std::cout << "\n";
  }

  return 0;
}
