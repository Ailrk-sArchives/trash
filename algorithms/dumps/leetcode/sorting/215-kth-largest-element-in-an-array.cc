#include <cassert>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

// k select. find the kth largest element in an unsorted array.
//
// Input: nums = [3,2,1,5,6,4], k = 2
// Output: 5

template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

// solution with quick select
namespace quick_select_sol {
class Solution {
public: /* quick_select */
  int lomuto_partition(vector<int> &nums, int l, int r) {
    int pivot = nums[r];
    int i = l;

    for (int j = l; j <= r; ++j) {
      // swap
      if (nums[j] > pivot) {
        swap(nums[i++], nums[j]);
      }
    }
    swap(nums[i], nums[r]);
    return i;
  }

  int quick_select(vector<int> &nums, int l, int r, int k) {
    int m = lomuto_partition(nums, l, r);
    int nth = m + 1;

    if (nth > k) {
      return quick_select(nums, l, m - 1, k);
    } else if (nth < k) {
      return quick_select(nums, m + 1, r, k);
    } else {
      return m;
    }
  }

  int findKthLargest(vector<int> &nums, int k) {
    int mid = quick_select(nums, 0, nums.size() - 1, k);
    return nums[mid];
  }
};
} // namespace quick_select

namespace priority_queue_sol {

// or you can just use stl's priority queue
class Solution {
public:
  int findKthLargest(vector<int> &nums, int k) {
    std::priority_queue<int> q;
    for (auto &n : nums)
      q.push(n);
    for (; k > 1; --k)
      q.pop();
    return q.top();
  }
};

} // namespace priority_queue

template <typename S> void test_cases(S solution) {
  {
    vector<int> v{3, 2, 1, 5, 6, 4};
    int res = solution.findKthLargest(v, 2);
    std::cout << res << std::endl;
    assert(res == 5);
    std::cout << "done" << std::endl;
  }

  // 6 5 5 4 3 3 2 2 1
  //       |
  {
    vector<int> v{3, 2, 3, 1, 2, 4, 5, 5, 6};
    int res = solution.findKthLargest(v, 4);
    std::cout << res << std::endl;
    assert(res == 4);
    std::cout << "done" << std::endl;
  }
}

int main(void) {
  {
    using namespace quick_select_sol;

    std::cout << "[quick select]: " << std::endl;
    Solution solution;
    test_cases(solution);
  }

  {
    using namespace priority_queue_sol;
    std::cout << "[priority queue]: " << std::endl;
    Solution solution;
    test_cases(solution);
  }

  return 0;
}
