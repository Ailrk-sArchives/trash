#include <iostream>
#include <set>
#include <cassert>
#include <vector>

int main(void) {
  std::set<int> ins{1, 2, 3};
  std::set<int> nono{};
  std::vector<std::set<int>> vsnono;

  std::cout << nono.empty() << std::endl;
  std::cout << (ins.find(1) == ins.end()) << std::endl;

  for (int i = 0; i < vsnono.size(); ++ i) {
    assert(vsnono[i].empty());
  }

  return 0;
}
