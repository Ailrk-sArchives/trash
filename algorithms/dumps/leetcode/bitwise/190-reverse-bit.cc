#include <cassert>
#include <iostream>

class Solution {
public:
  uint32_t reverseBits(uint32_t n) {
    uint32_t result = 0;
    for (int i = 0; i < sizeof(uint32_t) * 8; ++i) {
      result <<= 1;
      result += n & 1;
      n >>= 1;
    }
    return result;
  }
};

int main(void) {
  Solution solution;

  {
    uint32_t n = 0b00000010100101000001111010011100;
    uint32_t result = solution.reverseBits(n);
    std::cout << result << std::endl;
    assert((result ^ 0b00111001011110000010100101000000) == 0);
  }

  {
    uint32_t n = 0b11111111111111111111111111111101;
    uint32_t result = solution.reverseBits(n);
    std::cout << result << std::endl;
    assert((result ^ 0b10111111111111111111111111111111) == 0);
  }

  return 0;
}

