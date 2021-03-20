#include <iostream>
#include <string>
#include <string_view>

void weird() {
  constexpr std::string_view unicode[]{"▀▄─", "▄▀─", "▀─▄", "▄─▀"};
  for (int y{}, p{}; y != 20; ++y, p = ((p + 1) % 4)) {
    for (int x{}; x != 16; ++x) {
      std::cout << unicode[p];
    }
    std::cout << "\n";
  }
}

int main(void) {
  std::string s = "this is a long string";

  std::string_view sv{s};
  std::cout << sv.size() << std::endl;

  weird();
  return 0;
}
