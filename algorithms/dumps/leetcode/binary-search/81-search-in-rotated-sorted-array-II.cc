#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

// problem:
//   binary search in a rotated array.
// this is testing your case analysis. Because there is still ascending order,
// a rotated array can be considered as two parts
// [...rotated part, ...ordered part]

// [2, 5, 6, 0, 0, 1, 2], 0

class Solution {
public:
    bool search(vector<int>& nums, int target) {
      int l = 0, r = nums.size() - 1;
      while (l <= r) {
        int mid = (l + r) / 2;
        if (nums[mid] == target) return true;

        // in continous seq, can't tell which side is ascending
        if (nums[l] == nums[mid]) ++l;
        else if (nums[mid] <= nums[r]) {
          // right interval is ascending
          if (target > nums[mid] && target <= nums[r]) l = mid + 1;
          else r = mid - 1;
        } else {
          // left interval is ascending
          if (target >= nums[l] && target < nums[mid]) r = mid - 1;
          else l = mid + 1;
        }
      }
      return false;
    }
};

int main()
{
  Solution solution;

  {
    vector<int> v{2, 5, 6, 0, 0, 1, 2};
    auto res = solution.search(v, 0);
    assert(res);
    std::cout << res << std::endl;
  }

  {
    vector<int> v{2, 5, 6, 0, 0, 1, 2};
    auto res = solution.search(v, 3);
    assert(!res);
    std::cout << res << std::endl;
  }
}
