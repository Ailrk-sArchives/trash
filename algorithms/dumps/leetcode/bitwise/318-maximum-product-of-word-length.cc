#include <cassert>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Solution {
public:
  int maxProduct(vector<string> &words) {
    // TODO

  }
};

void test_cases(auto solution) {
  {
    std::vector<std::string> words{"abcw", "baz",  "foo",
                                   "bar",  "xtfn", "abcdef"};
    auto result = solution.maxProduct(words);
    std::cout << result << std::endl;
    assert(result == 4);
  }

  {
    std::vector<std::string> words{"a", "ab", "abc", "d", "cd", "bcd", "abcd"};
    auto result = solution.maxProduct(words);
    std::cout << result << std::endl;
    assert(result == 4);
  }
}

int main() {
  Solution solution;
  test_cases(solution);
}
