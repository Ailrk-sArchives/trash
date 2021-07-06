#include <cassert>
#include <iostream>
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
void merge(BidirectionalIterator first, BidirectionalIterator mid,
           BidirectionalIterator last, Comparator comp) {
  std::vector<typename std::iterator_traits<BidirectionalIterator>::value_type>
      tmp(0, last - first);

  std::cout << ": ";
  print_iter(first, last);
  BidirectionalIterator i = first, j = mid;

  while (i < mid || j < last) {
    if (comp(*i, *j)) {
      tmp.push_back(*i);
      tmp.push_back(*j);
    } else {
      tmp.push_back(*j);
      tmp.push_back(*i);
    }
    i++;
    j++;
  }
  while (i == mid && j < last) {
    tmp.push_back(*j);
    ++j;
  }

  while (i < mid && j == last) {
    tmp.push_back(*i);
    ++i;
  }

  for (auto iter = tmp.begin(); iter != tmp.end();)
    *first++ = *iter++;
}

// impl use close interval
template <typename BidirectionalIterator, typename Comparator>
void merge_sort_impl(BidirectionalIterator first, BidirectionalIterator last,
                     Comparator comp) {
  if (last - first <= 1) {
    return;
  }

  BidirectionalIterator mid = first + std::distance(first, last) / 2;
  std::cout << "mid: " << *mid << std::endl;
  merge_sort_impl(first, mid - 1, comp);
  merge_sort_impl(mid, last, comp);
  merge(first, mid, last, comp);
}

template <typename BidirectionalIterator, typename Comparator>
void merge_sort(BidirectionalIterator first, BidirectionalIterator last,
                Comparator comp) {
  merge_sort_impl(first, last - 1, comp);
}

int main(void) {
  {
    std::vector<int> v{5, 2, 4, 9, 1, 3, 8};
    merge_sort_impl(v.begin(), v.end(), [](auto a, auto b) { return a < b; });
    print_seq(v);
  }
  return 0;
}
