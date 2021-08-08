#include <iostream>
// https://www.foonathan.net/special-member-chart/

// special member funcitons:
// 1. default constructor
// 2. copy constructor
// 3. move constructor
// 4. copy assignment
// 5. move assignment
// 6. destructor

////////////////////////////////////////////////////////////
// Relies on the compiler generated defaults.
class Normal {
  Normal();

  // ~Normal() = default;
  // Normal(const Normal&) = default;
  // Normal(Normal&&) = default;

  // Normal& operator=(const Normal&) = default;
  // Normal& operator=(Normal&&) = default;
};


////////////////////////////////////////////////////////////
// Big five
//
// In case you need a user defined destructor to free memory allocated in heap.
// The default copy constructor will only copy the handler instead of the heap
// content. You need to provide copy constructor yourself.
//
// Once you have copy constructor declared the default move is deleted. you
// need to provide them as well.
class Container
{
public:
    Container() noexcept;
    ~Container() noexcept;

    Container(const Container& other);
    Container(Container&& other) noexcept;

    Container& operator=(const Container& other);
    Container& operator=(Container&& other) noexcept;
};

////////////////////////////////////////////////////////////
// Move only class
//
// e.g a wrapper of a file handle. The the ownership of the wrapper is
// always moved to whoever takes it.
class ResourceHandle {
  ResourceHandle() noexcept;
  ~ResourceHandle() noexcept;

  ResourceHandle(ResourceHandle&& other) noexcept;
  ResourceHandle& operator=(ResourceHandle&& other) noexcept;
};


// Create an object that is always stay at the same address
// delete copy constructor so compiler will not generate move constructor.
class Immovable {
  Immovable();
  // ~Immovable() = default;

  Immovable(const Immovable&) = delete;
  Immovable& operator=(const Immovable&) = delete;

  /* Immovable(Immovable&&) = delete; */
  /* Immovable& operator=(const Immovable&) = delete; */
};
