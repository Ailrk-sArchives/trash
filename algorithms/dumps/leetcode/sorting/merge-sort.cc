#include <cassert>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
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
// merge sort
// recursively divide array into half until it's small enough to be solved
// in one step.
// Then merge small, merged arrays.
// two partitions should always side by side
// note it's very hard to implement in place merge sort, you need to copy
// elements, which is a little bit unfortunate.

template <typename T>
void merge(vector<T> &nums, int left, int mid, int right) {
  const int lhs_sz = mid - left + 1;
  const int rhs_sz = right - mid;
  const int sz = right - left + 1;

  assert(lhs_sz + rhs_sz == sz);

  vector<int> lhs(lhs_sz + 1, 0);
  vector<int> rhs(rhs_sz + 1, 0);

  for (int i = 0; i < lhs_sz; ++i) {
    lhs[i] = nums[left + i];
  }
  lhs[lhs_sz] = INT32_MAX; // guard

  for (int i = 0; i < rhs_sz; ++i) {
    rhs[i] = nums[mid + 1 + i];
  }
  rhs[rhs_sz] = INT32_MAX;
  int i = 0, j = 0, k = 0;

  for (k = 0; k < sz; ++k) {
    if (lhs[i] < rhs[j])
      nums[k + left] = lhs[i++];
    else
      nums[k + left] = rhs[j++];
  }
}

template <typename T> void merge_sort(vector<T> &nums, int left, int right) {
  if (right - left + 1 <= 1)
    return;
  int mid = left + (right - left) / 2; // get mid without overflow.
  merge_sort(nums, left, mid);
  merge_sort(nums, mid + 1, right);
  merge(nums, left, mid, right);
}

int main(void) {

  {
    std::cout << "merge sort " << std::endl;
    vector<int> v{5, 2, 9, 1, 7, 10, 3, 4, 6};
    merge_sort(v, 0, v.size() - 1);
    print_seq(v);
  }

  return 0;
}
