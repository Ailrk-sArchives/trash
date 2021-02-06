#include <algorithm>
#include <array>
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
// !!!!
// This doesn't work because we are calling the vector's
// constructor with an initializer list,
// and an inisitalizer list only allows const access to it's elements,
// so the vector must copy it's elements.
// std::vector<std::unique_ptr<Shape>> shapes{
//   std::move(std::make_unique<Circle>()),
//   std::move(std::make_unique<Circle>()),
//   std::move(std::make_unique<Circle>()),
// };

std::vector<std::unique_ptr<Shape>> shapes = []() {
  std::vector<std::unique_ptr<Shape>> v;
  v.push_back(std::make_unique<Circle>());
  v.push_back(std::unique_ptr<Circle>(new Circle()));
  v.push_back(std::make_unique<Circle>());
  v.push_back(std::make_unique<Circle>());
  return v;
}();

// why one might want to use a red black tree instead of a
// hash map though?
// Hash table gives you O(1) looks up anyway.
// Hashtable doesn't preserve order. This only can evict a lot of
// use cases for hashtables.
// Apparently hashtable's performance is not very stable.
// Whenever there is a resize, the perfoamce plumment suddenly.
// Hash table also has poor locality, since data scattered around.
// If you consider putting things into a cacheline, hashtable might
// behave very bad.
std::map<std::string, std::array<int, 6>> phonebook_rbt{
    {"Jimmy", {1, 1, 1, 1, 1, 1}},
    {"Sam", {1, 1, 1, 1, 1, 1}},
    {"Lil", {1, 1, 1, 1, 1, 1}},
};

int main(void) {
  // set phonebook with a constant size.
  phone_book.reserve(30);
  phone_book.push_back({"Jimmy", {1, 2, 3, 4, 1, 2}});

  // handle the out of range exception.
  {
    int index = 1;
    try {
      std::cout << "shapes has size: " << shapes.size() << std::endl;

      auto v = std::move(shapes.at(3));
      std::cout << (v->size()) << std::endl;

    } catch (std::out_of_range e) {
      std::cout << e.what() << std::endl;
    }
  }

  {
    // the first element of shapes get moved out.
    auto c = std::move(shapes.at(0));
    std::cout << "shapes[0] moved out" << (shapes[0] == nullptr) << std::endl;
  }

  {
    // shapes1 is the new onwer of vectors hold by shape.
    decltype(shapes) shapes1{std::move(shapes)};

    // from this point onwards shapes is nullptr.
    std::cout << "shapes1[0] moved out" << (shapes1[0] == nullptr) << std::endl;
  }

  // here shapes is nolonger validd, but somehow you can still access it. jee.

  return 0;
}
