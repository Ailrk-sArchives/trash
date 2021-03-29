#include <string>
#include <unordered_map>
#include <iostream>
#include <vector>

struct Good {
  std::vector<int> vood;
};

int main(void) {
  std::vector<std::tuple<std::string, Good>> vs{{"1", Good{}}, {"2", Good{}}};
  std::unordered_map<std::string, Good> m;

  for (auto &[k, v] : vs) {
    m.emplace(k, v);
  }

  std::cout << m.bucket_count() << std::endl;
  std::cout << m.max_load_factor() << std::endl;

  return 0;
}
