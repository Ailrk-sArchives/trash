#include <iostream>
#include <vector>
#include <functional>

using namespace std;

//////////////////////////////////////////////////////////////////////////////
// quicksort:
//   given array A, l, r be lower and upper bound of range to sort
//   steps:
//     1. pick a pivot where l < p < r
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
auto hoare_partition = [](auto& nums, int l, int r) {
  int pivot = nums[l];
  int i = l - 1;
  int j = r + 1;

  for (;;) {
    do i++; while (nums[i] < pivot);
    do j--; while (nums[j] > pivot);
    if (i >= j) break;
    swap(nums[i], nums[j]);
  }
  return j;
};


// Lomuto paritition scheme
// set the first
auto lomuto_partition = [](auto &nums, int l, int r) {
  int pivot = nums[l];
  int i = l + 1;
  for (int j = l;  j < r; ++j) {
    if (nums[j] < pivot) {
      swap(nums[j], nums[i]);
      ++i;
    }
  }
  swap(nums[l], nums[i - 1]);
  return i;
};

template <typename T, typename Partition>
void quick_sort(vector<T> &nums, int l, int r, Partition partition) {
  if (l >= r) return;
  int p = partition(nums, l, r);  // return the index of the partition
  quick_sort(nums, l, p, partition);
  quick_sort(nums, p + 1, r, partition);
}

// quick sorts
template <typename T>
void quick_sort(vector<T> &nums, int l, int r) {
  quick_sort(nums, l, r, hoare_partition);
}


//////////////////////////////////////////////////////////////////////////////
// merge sort
// recursively divide array into half until it's small enough to be solved
// in one step.
// Then merge small, merged arrays.
template <typename T>
void merge_sort(vector<T> &nums, int l, int r, vector<int> &tmp) {
  if (l + 1 >= r) return;
  int m = l + (r - 1) / 2;
  merge_sort(nums, l, m, tmp);
  merge_sort(nums, m, r, tmp);

  int p = l, q = m, i = l;
  while (p < m || q < r) {
    if (q >= r || (p < m && nums[p] <= nums[q])) {
      tmp[i++] = nums[p++];
    } else {
      tmp[i++] = nums[q++];
    }
  }
  for (int i = l; i < r; ++i) {
    nums[i] = tmp[i];
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
template <typename T>
void insertion_sort(vector<T>& nums, int n) {
  for (int i = 0; i < n; ++i) {
    for (int j = i; j > 0 && nums[j] < nums[j - 1]; --j) {
      swap(nums[j], nums[j - 1]);
    }
  }
}


//////////////////////////////////////////////////////////////////////////////
// bubble sort
template <typename T>
void bubble_sort(vector<int>& nums, int n) {
  bool swapped;
  for (int i = 1; i < n; ++i) {
    swapped = false;
    for (int j = 1; j < n - i + 1; ++j) {
      swap(nums[j], nums[j - 1]);
      swapped = true;
    }
    if (!swapped) break;
  }
}


//////////////////////////////////////////////////////////////////////////////
// insertion sort
template <typename T>
void selection_sort(vector<int>& nums, int n) {
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


template <typename C>
void print_seq(C &&container) {
  for (auto& n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

int main(void)
{

  {
    std::cout << "quick sort [hoare partition]" << std::endl;
    vector<int> v{5, 2, 9, 1, 7, 10, 3};
    quick_sort(v, 0, v.size() - 1);
    print_seq(v);

    v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    quick_sort(v, 0, v.size() - 1);
    print_seq(v);
  }

  /* { */
  /*   std::cout << "quick sort [lomuto paritition]" << std::endl; */
  /*   vector<int> v{5, 2, 9, 1, 7, 10, 3}; */
  /*   quick_sort(v, 0, v.size() - 1, lomuto_partition); */
  /*   print_seq(v); */

  /*   v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}; */
  /*   quick_sort(v, 0, v.size() - 1, lomuto_partition); */
  /*   print_seq(v); */
  /* } */


  return 0;
}
