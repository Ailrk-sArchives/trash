#include <algorithm>
#include <iostream>
#include <memory>
#include <vector>

// c++ data models with constructors and destructors.
// The idea of having class is to make it easier to model real world problem.
// But when they need to run on the computer, how to efficiently manage their
// resources is also a problem we need to care about.
//
// c++ address this problem with RAII, you create an object by calling a
// constructor, and delete the object by calling it's destructor. Each object
// has their lifetime, within the lifetime the object will obtain resources, do
// it's work, free resources, and everything get managed properly.
//
// Constructor is nothing special but a function. By calling it you bring a new
// object into life with resources they require.
//
// What're resources? Say your class has two integer fields, so it will need to
// allocate 16 bytes to store those integers. The 16 memory space is an example
// of resource you acquire.
//
// The object can both be create on the heap or on the stack, but they will
// follow the same memory layout, and be created with the same procedure
// specified by the constructor.
//
// Among all the constructors you can define, there are two special
// constructors, which will be called by the compiler when their corresponding
// semantics get triggered.
//
// One is copy constructor, the other is move constructor.
// The common pattern is you pass another object (refernce, no matter lval ref
// or rval ref, they always ref to another obect with the same type) into the
// constructor, and the constructor decides to do something with the other
// object to create itself.
//
// Normally you get another object, you might want to copy it. You want to have
// another copy of the exact same memory layout, socket refs, etc as the object
// get passed in. Or maybe sometimes you don't want to copy the entire object
// but only part of it. These are all things you can specify in a copy
// construtor.
//
// What if you want to take over the object get passed in? Then you need the
// move semantics, steal resources from the other obejct directly.

// Notice for unique_ptr, it essentially still a object on the stack holds a
// poiner. Because it is a compile time construct, we can optimize it out and
// make it as efficient as a raw pointer. Of course copy an unique_ptr is not
// allowed. So what does it means to copy a unique_ptr? If you copy the
// unique_ptr, you probably want to copy the member field from one unique_ptr to
// another. And that make the object get pointed to by the member field an
// alias, no longer unique anymore.

// Let's make a class with both copy and move constructor, and see what happens.

class MemoryBlock {
  size_t size;
  int *data;

public:
  // first just make a simple constructor.
  // This constructor has no difference between define a fuction and
  // return MemoryBlock.
  explicit MemoryBlock(size_t size) : size(size), data(new int[size]) {
    std::cout << "Create memory block size: " << size << std::endl;
  }

  // because we now holds the array, we need corresponding destructor to delete
  // it. if yo don't define this, compiler will do it for you.
  ~MemoryBlock() {
    std::cout << "Deleting memory block, size: " << size << std::endl;
    if (data != nullptr) {
      delete[] data;
    }
  }

  // copy constructor
  MemoryBlock(const MemoryBlock &other)
      : size(other.size), data(new int[other.size]) {
    std::copy(other.data, other.data + size, data);

    std::cout << "copy construct a memory block size:" << size << std::endl;
  }

  // copy assignment
  MemoryBlock &operator=(const MemoryBlock &other) {

    // from here you can see & is merely a reference.
    if (this != &other) {
      size = other.size;
      data = new int[size];
      std::copy(other.data, other.data + size, data);

      std::cout << "copy assign a memory block" << size << std::endl;
    }
    return *this;
  }

  // move semantics
  // first initialze empty members.
  MemoryBlock(MemoryBlock &&other) : data(nullptr), size(0) {

    // steal the data directly
    data = other.data;

    // this is still copy, but it's primitive type so it's ok.
    size = other.size;

    // null out the other object.
    // This prevents the destructor from freeing resources multiple times.
    other.data = nullptr;
    other.size = 0;
    // What will be the problem if you don do this?
    // for example in case { A test(std::move(getA()));},
    // move getA() creates a rvalue, moved into test and get destroyed.
    // and after reach the end of the block test get destroyed again.
    std::cout << "Move memory block, size: " << size << std::endl;
  }

  // move assignment is very straight forward.
  MemoryBlock &operator=(MemoryBlock &&other) {

    data = other.data;
    size = other.size;
    other.data = nullptr;
    other.size = 0;

    std::cout << "Move assign memory block, size: " << size << std::endl;
    return *this; // return a reference
  }
};

void ad() {
  // define struct within function..
  struct R {
    double a, b, c;
  };
  static R rs[2] = {{1, 1, 1}, {1, 2, 3}};

  for (int i = 0, k = 0; i < 2; ++i) {
    rs[i] = {rs[i].a + 1, rs[i].b + 1, rs[i].c + 1};
  }
}

int main(void) {
  std::vector<MemoryBlock> v;

  // a lval reference.
  MemoryBlock m(18);
  MemoryBlock n(10);

  // move
  v.push_back(MemoryBlock(25));
  // copy
  v.push_back(m);

  // move
  // What does move do is a conditional cast.
  // it casts lvalue to rvalue, and keep rvalue
  v.push_back(std::move(n));

  return 0;
}
