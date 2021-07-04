// https://leetcode.com/problems/merge-sorted-array/

#include <iostream>
#include <vector>

using namespace std;

// Steps:
//  get
//    1. the pointer points the end of the first array;
//    2. two pointers points to the end of valid arrays, m-1, n-1
//  comparing n1[m-1] with n2[n-1], put the bigger one to the end of the pos,
//  decrement pos.
//  decrement m and n;
//
// Claim:
//   [1] at beginning:
//     1. bigger element at the end of n1 and n2 is the biggest element of
//        all elements.
//     2. n1[pos] is the biggest elment
//
//   [2] at each time we choose an element to put at the end:
//      1. we decrement the pointer to the larger element, and update it
//         to the previous element,
//            as argmaxmax(n1[m-1], n2)[n-1]) - 1
//          The problem remained is still the same problem.
//
//   [3] at the end
//      The algorihtm stops when both m and n points to 0.
//      invariant in [3] holds inductively, so at the end elements in n1 is
//      in increasing order.

class Solution {
public:
    void merge(vector<int>& nums1, int m, vector<int>& nums2, int n) {
      int pos = m + n - 1;
      m--; n--;
      while (m >= 0 && n >= 0) {
        nums1[pos--] = nums1[m] > nums2[n] ? nums1[m--] : nums2[n--];
      }
      while (n >= 0) {
        nums1[pos--] = nums2[n--];
      }
    }
};


int main(void)
{
  Solution solution;

  {
    vector<int> v1{1, 2, 3, 0, 0, 0};
    vector<int> v2{2, 5, 6};
    solution.merge(v1, 3, v2, 3);
    for (auto& n: v1) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }

  {
    vector<int> v1{1};
    vector<int> v2{};
    solution.merge(v1, 1, v2, 0);
    for (auto& n: v1) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }

  {
    vector<int> v1{0};
    vector<int> v2{1};
    solution.merge(v1, 0, v2, 1);
    for (auto& n: v1) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }
  return 0;
}
