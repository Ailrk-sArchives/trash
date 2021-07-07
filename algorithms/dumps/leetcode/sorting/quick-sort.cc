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
// quicksort:
//   given array A, left, right be lower and upper bound of range to sort
//   steps:
//     1. pick a pivot where left < p < right
//     2. the pivot partitioned A into two parts,
//        move elements e < A[p] to one side, otherwise the other side.
//     3. recursively apply quicksort to sub ranges.
// performance of quick sort depends on the partition algorithm choosed. There
// are many variations for paritioning.
//
// - choice of pivot

// Hoare partition scheme
// this is the original partition scheme used by tony hoare.
//
// In early version of quick sorts, left most element is usually chosen
// as the pivot element. (which gives the worst case in sorted array)
// To avoid this problem, we can pick the pivot randomly.
// This versoin uses median, which only avoid worst case in sorted array.
// NOTE: it's not necessary for the pivot to be at the middle position.
auto hoare_partition = [](auto &nums, int left, int right) {
  int pivot = nums[left];
  int i = left;
  int j = right;

  for (;;) {
    while (nums[i] < pivot)
      i++;
    while (nums[j] > pivot)
      j--;
    if (i >= j)
      break;
    swap(nums[i], nums[j]);
  }
  return j;
};

// Lomuto paritition scheme
// it mantains two indicies nums[i], nums[j]. i = j = left at the beginning.
// scan the array by advancing j.
// do nothing when nums[j] >= pivot,
// swap the value of nums[i] and nums[j] when nums[j] < pivot
// this brings numbers smaller than pivot to the left and numbers larger then
// the pivot to the right.
// [claim] when j = right - 1, i points to the last number that bigger then the
//         pivot.
// [claim] if we swap the pivot and nums[i] after the loop finished, we have
//             (forall e in nums[0:i-1], e < pivot)
//         and (forall e in nums[i:right] e > pivot)
// set the first
//
// lomuto paritition makes more  swap than hoare's partition, because each
// time nums[i] get swaped
auto lomuto_partition = [](auto &nums, int left, int right) {
  int pivot = nums[right]; // choose
  int i = left;
  for (int j = left; j <= right; ++j) {
    if (nums[j] < pivot) {
      swap(nums[j], nums[i]);
      ++i;
    }
  }
  swap(nums[i], nums[right]);
  return i;
};

template <typename T, typename Partition>
void quick_sort(vector<T> &nums, int left, int right, Partition partition) {
  if (left >= right)
    return;
  int p = partition(nums, left, right);     // return the index of the partition
  quick_sort(nums, left, p - 1, partition); // note we don't need mid
  quick_sort(nums, p + 1, right, partition);
}

// quick sorts
template <typename T> void quick_sort(vector<T> &nums, int left, int right) {
  quick_sort(nums, left, right, hoare_partition);
}

int main(void) {
  {
    std::cout << "quick sort [hoare partition]" << std::endl;
    vector<int> v{5, 2, 9, 1, 7, 10, 3};
    quick_sort(v, 0, v.size() - 1);
    print_seq(v);

    v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    quick_sort(v, 0, v.size() - 1);
    print_seq(v);
  }

  {
    std::cout << "quick sort [lomuto paritition]" << std::endl;
    vector<int> v{5, 2, 9, 1, 7, 10, 3};
    quick_sort(v, 0, v.size() - 1, lomuto_partition);
    print_seq(v);

    v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    quick_sort(v, 0, v.size() - 1, lomuto_partition);
    print_seq(v);
  }

  return 0;
}
