#include <iostream>
#include <cassert>

// xxxx10
class Solution {
public:
    bool isPowerOfFour(int n) {

      // TODO
    }
};


int main(void)
{

  Solution solution;

  {
    int n = 16;
    int result = solution.isPowerOfFour(n);
    std::cout << result << std::endl;
    assert(result);
  }

  {
    int n = 5;
    int result = solution.isPowerOfFour(n);
    std::cout << result << std::endl;
    assert(!result);
  }

  {
    int n = 1;
    int result = solution.isPowerOfFour(n);
    std::cout << result << std::endl;
    assert(result);
  }


  return 0;
}
