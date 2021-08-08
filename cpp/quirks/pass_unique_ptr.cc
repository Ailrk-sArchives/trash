#include <iostream>
#include <memory>

//////////////////////////////////////////////////////////////
// By value
struct Base1 {
  std::unique_ptr<Base1> next;
  Base1() : next(nullptr) {}
  Base1(std::unique_ptr<Base1> n) : next(std::move(n)) {}
};

void test_Base1() {
  auto base1_1 = std::make_unique<Base1>();

  // to pass unique ptr by value we have to pass a temporary.
  Base1 base1_2(std::make_unique<Base1>());

  // in this case we leave the base1_1 in a valid unspecified state, and content
  // of the unique_ptr is moved as temporary bind to the parameter.
  //
  // inside the constructor the rvalue is moved again and bind to the lvalue
  // next.
  Base1 base1_3(std::move(base1_1));
}

//////////////////////////////////////////////////////////////
// By non const lvalue reference
struct Base2 {
  std::unique_ptr<Base2> next;
  Base2() : next(nullptr) {}
  Base2(std::unique_ptr<Base2> &n) : next(std::move(n)) {}
};

// we have to pass a lvalue reference. The reference is passed and get moved,
// the move from object is now in unspecified state.
void test_Base2() {
  auto ptr = std::make_unique<Base2>();
  Base2 base{ptr};
}

//////////////////////////////////////////////////////////////
// pass by const lvalue reference
// In this case we can't move, because move cast from const T& to T&& will drop
// the const qualifier.
//
// To get a unique ptr from another unique ptr you essentially need to copy the
// content and construct another unique ptr.
// Actually this is toally valid because the copy semantic can still exist for
// the underlying object.
//
// If you want to convert a ptr semantic to value semantic, e.g for achieveing
// polymorphic value, you can preserve the copy semnatic by creating a new
// unique ptr points to the new copy of the value of another unique ptr.
struct Base3 {
  std::unique_ptr<Base3> next;
  Base3() : next(nullptr) {}
  Base3(const std::unique_ptr<Base3> &n) : next(std::make_unique<Base3>(*n)) {}
};

void test_Base3() {
  auto ptr = std::make_unique<Base3>();
  Base3 bse3_1(ptr);
}

// or really if you need to pass const reference of a unique ptr, just pass
// the dereferenced value by const reference.
struct Base3_ref {
  std::unique_ptr<Base3_ref> next;
  Base3_ref() : next(nullptr) {}
  Base3_ref(Base3_ref const &n) : next(std::make_unique<Base3_ref>(n)) {}
};

void test_Base3_ref() {
  auto ptr = std::make_unique<Base3_ref>();
  Base3_ref base3_1(*ptr);
}

//////////////////////////////////////////////////////////////
// By rvaleu reference
// it's similar as to move by non const reference, it just move when it's passed
// as paramter instead of move the reference when contructing the next ptr.

struct Base4 {
  std::unique_ptr<Base4> next;
  Base4() : next(nullptr) {}
  Base4(std::unique_ptr<Base4> &&n) : next(std::move(n)) {}
};

// now you need to move it when passing it.
void test_Base4() {
  auto ptr = std::make_unique<Base4>();
  Base4 base{std::move(ptr)};
}
