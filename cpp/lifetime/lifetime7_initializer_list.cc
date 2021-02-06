#include <array>
#include <iostream>
#include <string>
#include <vector>

// initalizer_list<> invocations create hiden const arrays.

int main(void) {

  // one initialization. "a" and "b" are small string optimized.
  std::vector<std::string> v1{"a", "b"};

  // 5 initialization
  // two string initialized,
  // then vector get initialized,
  // then two string get copied to vector
  std::vector<std::string> v2{"aaaaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbbbbbb"};

  // 0 initialization
  // standard array doesn't have any constructors.
  // And it will not use a initilizer list.
  std::array<std::string, 2> arr{"aaaaaaaaaaaaaaaaaaaa",
                                 "aaaaaaaaaaaaaaaaaaaa"};

  return 0;
}
