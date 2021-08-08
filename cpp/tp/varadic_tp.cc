#include <algorithm>
#include <iostream>
#include <sstream>
#include <string_view>

template <typename... Args,
          typename Enable = std::enable_if_t<
              std::conjunction_v<std::is_same<std::string, Args>...>>>
std::string print(Args... args) {
  std::string v;
  (v.append(args), ...);
  return v;
}

int main(void) {
  std::cout << print(std::string("asdl"), std::string("good")) << std::endl;

  return 0;
}
