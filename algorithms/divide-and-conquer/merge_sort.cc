#include <cassert>
#include <iostream>
#include <limits>
#include <type_traits>
#include <vector>

template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

template <typename It> void print_iter(It first, It last) {
  while (first != last) {
    std::cout << *first << " ";
    ++first;
  }
  std::cout << std::endl;
}

///////////////////////////////////////////////////////////////////////////////

template <typename BidirectionalIterator, typename Comparator>
void merge(BidirectionalIterator left, BidirectionalIterator mid,
           BidirectionalIterator right, Comparator &&comp) {
  const int lhs_sz = mid - left + 1;
  const int rhs_sz = right - mid;
  const int sz = lhs_sz + rhs_sz;

  std::vector<int> lhs(lhs_sz, 0);
  std::vector<int> rhs(rhs_sz, 0);

  for (auto l = left, lhs_ptr = lhs.begin(); l < left + lhs_sz;) {
    *lhs_ptr++ = *l++;
  }

  for (auto r = mid + 1, rhs_ptr = rhs.begin(); r < mid + 1 + rhs_sz;) {
    *rhs_ptr++ = *r++;
  }

  auto l = lhs.begin(), r = rhs.begin();
  for (auto k = left; k != right + 1; ++k) {

    assert(!(l == lhs.begin() + lhs_sz && r == rhs.begin() + rhs_sz));

    // left ends
    if (l == lhs.begin() + lhs_sz && r < rhs.begin() + rhs_sz) {
      *k = *r++;
      // right ends
    } else if (l < lhs.begin() + lhs_sz && r == rhs.begin() + rhs_sz) {
      *k = *l++;
    } else {
      if (comp(*l, *r))
        *k = *l++;
      else
        *k = *r++;
    }
  }
}

template <typename BidirectionalIterator, typename Comparator>
void merge_sort_impl(BidirectionalIterator first, BidirectionalIterator last,
                     Comparator &&comp) {
  if (last - first + 1 <= 1)
    return;

  auto mid = first + std::distance(first, last) / 2;
  merge_sort_impl(first, mid, comp);
  merge_sort_impl(mid + 1, last, comp);
  merge(first, mid, last, comp);
}

template <typename BidirectionalIterator, typename Comparator>
void merge_sort(BidirectionalIterator first, BidirectionalIterator last,
                Comparator &&comp) {
  merge_sort_impl(first, last - 1, comp);
}

int main(void) {

  {
    std::cout << "merge sort " << std::endl;
    std::vector<int> v{5, 2, 9, 1, 7, 10, 3, 4, 6};
    merge_sort(v.begin(), v.end(), [](auto a, auto b) { return a < b; });
    print_seq(v);
  }

  {
    std::cout << "merge sort " << std::endl;
    std::vector<int> v{5, 2, 9, 1, 7, 10, 3, 4, 6};
    merge_sort(v.begin(), v.end(), [](auto a, auto b) { return a > b; });
    print_seq(v);
  }

  return 0;
}
