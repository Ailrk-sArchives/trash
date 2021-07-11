#include <iostream>
#include <vector>
using namespace std;

template <typename C> void print_grid(C &&container) {
  if (container.empty()) {
    std::cout << "empty" << std::endl;
    return;
  }
  for (auto &n : container) {
    for (auto &s : n)
      std::cout << s << " ";
    std::cout << std::endl;
  }
  std::cout << std::endl;
}

// use the so called backtracking technique. We can mutate the state of the
// graph when we visiting a nodes, and set it's value back after the recursion
// is finished.

class Solution {
public:
  vector<vector<int>> permute(vector<int> &nums) {
    std::vector<std::vector<int>> result;
    backtracking(nums, 0, result);
    return result;
  }

  void backtracking(std::vector<int> &nums, int level,
                    std::vector<std::vector<int>> &result) {
    if (level == nums.size() - 1) {
      result.push_back(nums);
      return;
    }

    for (int i = level; i < nums.size(); ++i) {
      std::swap(nums[i], nums[level]); // modify the node
      backtracking(nums, level + 1, result);
      std::swap(nums[i], nums[level]); // change it back
    }
  }
};

int main() {
  Solution solution;

  {
    std::vector<int> nums{1, 2, 3};
    auto result = solution.permute(nums);
    print_grid(result);
  }

  {
    std::vector<int> nums{1, 0};
    auto result = solution.permute(nums);
    print_grid(result);
  }
}
