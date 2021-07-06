#include <cassert>
#include <cstring>
#include <functional>
#include <iostream>
#include <vector>

using namespace std;
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

//////////////////////////////////////////////////////////////////////////////
// bubble sort
template <typename T> void bubble_sort(vector<int> &nums, int n) {
  bool swapped;
  for (int i = 1; i < n; ++i) {
    swapped = false;
    for (int j = 1; j < n - i + 1; ++j) {
      swap(nums[j], nums[j - 1]);
      swapped = true;
    }
    if (!swapped)
      break;
  }
}

//////////////////////////////////////////////////////////////////////////////
// insertion sort
template <typename T> void selection_sort(vector<int> &nums, int n) {
  int mid;
  for (int i = 0; i < n - 1; ++i) {
    mid = i;
    for (int j = i + 1; j < n; ++j) {
      if (nums[j] < nums[mid]) {
        mid = j;
      }
    }
    swap(nums[mid], nums[i]);
  }
}

//////////////////////////////////////////////////////////////////////////////
// insertion sort
//   performs is good for small input, so it's often used as the base
//   case of merge sort or quick sort.
// steps:
//   two loops. The outer loop iterate over the array,
//   the inner loop loops from the current index backwards to the beginning and
//   swap with elements that is bigger then it, until it's bigger then the
//   previous element.
template <typename T> void insertion_sort(vector<T> &nums, int n) {
  for (int i = 0; i < n; ++i) {
    for (int j = i; j > 0 && nums[j] < nums[j - 1]; --j) {
      swap(nums[j], nums[j - 1]);
    }
  }
}
