#include <cassert>
#include <iostream>
#include <vector>

using namespace std;

class Solution {
public:
  int singleNumber(vector<int> &nums) {
    int result = 0;
    for (auto &v : nums) {
      result ^= v;
    }
    return result;
  }
};

int main(void) {
  Solution solution;

  {
    std::vector<int> v{2, 2, 1};
    int result = solution.singleNumber(v);
    std::cout << result << std::endl;
    assert(!(result ^ 1));
  }

  {
    std::vector<int> v{4, 1, 2, 1, 2};
    int result = solution.singleNumber(v);
    std::cout << result << std::endl;
    assert(!(result ^ 4));
  }
  return 0;
}
