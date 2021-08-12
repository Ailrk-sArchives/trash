#include <iostream>
#include <vector>

using namespace std;

namespace S1 {
class Solution {
public:
  int numSubarrayBoundedMax(vector<int> &nums, int left, int right) {
  }
};

}; // namespace S1

template <typename C> void prints(const C &c) {
  for (auto &n : c)
    std::cout << n << " ";
  std::cout << std::endl;
}

template <typename S> void test1(S &s) {}

int main(void) {

  {
    using namespace S1;
    Solution s;
    test1(s);
  }
  return 0;
}
