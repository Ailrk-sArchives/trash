#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

class Shape {
public:
  virtual ~Shape() = default;
  virtual size_t size() const = 0;
};

class Circle : public Shape {
public:
  size_t size() const override { return 10; }
};

template <unsigned int Digits> struct Entry {
  std::string name;
  unsigned int number[Digits];
};

std::vector<Entry<6>> phone_book{
    {"Jimmy", {5, 6, 7, 1, 2, 3}},
    {"Lucas", {5, 6, 7, 1, 2, 3}},
    {"Giles", {1, 2, 3, 4, 5, 6}},
};

// if you have polymorphis types, store poitners
// rather than type themselves, otherwise you will have
// slicing problems.
std::vector<std::unique_ptr<Shape>> shapes{
    std::make_unique<Circle>(),
    std::make_unique<Circle>(),
    std::make_unique<Circle>(),
};

int main(void) {
  // set phonebook with a constant size.
  phone_book.reserve(30);
  phone_book.push_back({"Jimmy", {1, 2, 3, 4, 1, 2}});

  {
    int index;
    std::cin >> index;
    try {
      std::cout << shapes[index]->size() << std::endl;
    } catch (std::out_of_range e) {
      std::cout << e.what() << std::endl;
    }
  }

  return 0;
}
