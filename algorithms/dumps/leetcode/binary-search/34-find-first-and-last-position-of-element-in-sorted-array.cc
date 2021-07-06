// https://leetcode.com/problems/find-first-and-last-position-of-element-in-sorted-array/

#include <iostream>
#include <vector>
using namespace std;


// Input: nums = [5,7,7,8,8,10], target = 8
// Output: [3,4]

// the problem reduce to find the upper bound and the lower bound of an
// instance in a sorted array with binary search.
//
// Steps:
//  use normal binary search.
//    when comparing the mid elemnt with the target element and decide in what
//    case we should do r = mid,
//      (nums[mid] >= target ⇒ r = mid) ⇒ find the lower bound.
//      (nums[mid] > target ⇒ r = mid) ⇒ find the upper bound.
//
// TODO proof

class Solution {
public:
    vector<int> searchRange(vector<int>& nums, int target) {
      int lb = lower_bound(nums, target);
      int ub = upper_bound(nums, target) - 1;
      if (lb == nums.size() || nums[lb] != target) {
        return {-1, -1};
      }
      return {lb, ub};
    }

    template <typename Comp>
    int bound(vector<int>& nums, int target, Comp comp) {
      int l = 0, r = nums.size();
      int mid;

      while (l < r) {
        mid = (l + r) / 2;
        if (comp(nums[mid], target)) {
          r = mid;
        } else {
          l = mid + 1;
        }
      }
      return l;
    }

    int lower_bound(vector<int>&nums, int target) {
      return bound(nums, target, [](auto a, auto b) {
          return a >= b;
          });
    }

    int upper_bound(vector<int>&nums, int target) {
      return bound(nums, target, [](auto a, auto b) {
          return a > b;
          });
    }
};


int main(void)
{
  Solution solution;

  {
    vector<int> v{5, 7, 7, 8, 8, 10};
    auto res = solution.searchRange(v, 8);

    for (auto& n : res) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }

  {
    vector<int> v{5, 7, 7, 8, 8, 10};
    auto res = solution.searchRange(v, 6);

    for (auto& n : res) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }

  {
    vector<int> v{};
    auto res = solution.searchRange(v, 0);

    for (auto& n : res) {
      std::cout << n << " ";
    }
    std::cout << std::endl;
  }

  return 0;
}
