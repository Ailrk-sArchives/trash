#include <iostream>
#include <limits>
#include <vector>

#define PRINT(name) std::cout << #name << " " << name << std::endl;

int main(void) {

  PRINT(INT8_MAX);
  PRINT(INT16_MAX);
  PRINT(INT32_MAX);
  PRINT(INT64_MAX);

  PRINT(INT8_MIN);
  PRINT(INT16_MIN);
  PRINT(INT32_MIN);
  PRINT(INT64_MIN);

  PRINT(UINT8_MAX);
  PRINT(UINT16_MAX);
  PRINT(UINT32_MAX);
  PRINT(UINT64_MAX);

  std::vector<int> v{1, 2, 3, 0};
  v[v.size() - 1] = INT32_MAX;
  for (auto &e : v)
    std::cout << e << " ";
  std::cout << std::endl;
  return 0;
}
