#include <iostream>

// default constructors and their behaviors.

// is default move constructor equals member wise move?
// https://stackoverflow.com/questions/18290523/is-a-default-move-constructor-equivalent-to-a-member-wise-move-constructor

// not, compiler has specific knowledge of class(const class&) as copy
// consructor.
struct nonmovable {
  nonmovable() = default;
  nonmovable(const nonmovable &) = default;
  nonmovable(nonmovable &&) = delete;
};

struct movable {
  movable() = default;
  movable(const movable &) { std::cout << "copy" << std::endl; }
  movable(movable &&) { std::cout << "move" << std::endl; }
};

struct has_nonmovable {
  movable a;
  nonmovable b;

  has_nonmovable() = default;
  has_nonmovable(const has_nonmovable &) = default;
  has_nonmovable(has_nonmovable &&) = default;
};

int main(void) {

  // has_nonmovable has an not movable field.
  // so it's default move constructor will do a member wise 1copy instead...
  has_nonmovable c;
  has_nonmovable d(std::move(c));
  return 0;
}
