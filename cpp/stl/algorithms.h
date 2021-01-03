#include <algorithm>
#include <iostream>
#include <vector>

template <unsigned int Digits> struct Entry {
  std::string name;
  unsigned int number[Digits];
};

std::vector<Entry<6>> phone_book1{
    {"Jimmy", {5, 6, 7, 1, 2, 3}},
    {"Lucas", {5, 6, 7, 1, 2, 3}},
    {"Giles", {1, 2, 3, 4, 5, 6}},
};

std::vector<Entry<6>> phone_book2{
    {"Bob", {5, 6, 7, 1, 2, 3}},
    {"Rick", {5, 6, 7, 1, 2, 3}},
    {"MM", {1, 2, 3, 4, 5, 6}},
    {"MM", {1, 2, 3, 4, 5, 6}},
};

template <typename T, typename Comp>
std::vector<T> make_set(std::vector<T> &xs, std::vector<T> &ys, Comp cmp) {
  std::vector<T> vs;
  std::unique_copy(xs.begin(), xs.end(), std::back_inserter(vs), cmp);
  std::unique_copy(ys.begin(), ys.end(), std::back_inserter(vs), cmp);
  std::sort(vs.begin(), vs.end(), cmp);
  return vs;
}


int main(void) {
  auto vs = make_set(phone_book1, phone_book2, std::greater<Entry<6>>());
  for (auto &v : vs) {
    std::cout << v.name << ", " << v.number << std::endl;
  }
  return 0;
}
