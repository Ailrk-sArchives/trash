// https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/

#include <iostream>
#include <vector>
#include <iterator>

// two pointesr problem
// It's ascking for a sum of two number l, r, we can move two pointers from
// two end of the array and check the sum until they overlap.
// Because the array is sorted, n[l] < n[r]. Thus if n[l] + n[r] < target,
// we need a slightly bigger number, thus l++.
// Conversely if sum > target, we need to remove the bigger numer thus r--;
//
// Claim: the targe sum must be one of combination of l, r

using namespace std;

class Solution {
public:
    vector<int> twoSum(vector<int>& numbers, int target) {

      int l = 0;
      int r = numbers.size() - 1;

      while (l < r) {
        int sum = numbers[l] + numbers[r];
        if (sum == target) break;
        if (sum < target) ++l;
        else --r;
      }

      return { l + 1, r + 1 };
    }
};


int main(void)
{

  Solution solution;

  {
    vector<int> v {2, 7, 11, 15};
    auto res = solution.twoSum(v, 9);

    for (auto& n : res) {
      std::cout << n << " " << std::endl;
    }
  }

  {
    vector<int> v {2, 3, 4};
    auto res = solution.twoSum(v, 6);

    for (auto& n : res) {
      std::cout << n << " " << std::endl;
    }
  }

  {
    vector<int> v {-1, 0};
    auto res = solution.twoSum(v, -1);

    for (auto& n : res) {
      std::cout << n << " " << std::endl;
    }
  }
  return 0;
}
