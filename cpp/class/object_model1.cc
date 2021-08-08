#include <cmath>
#include <iostream>

// what does a struct compiles into?
// 1. members are stored in the struct directly.
// 2. static member and static functions are hoisted out of the class and
//    defeind in data section.
// 3. virtual function will form a virtual table in data section. Compiler will
//    insert a vptr in the struct. Call to the function at runtime will follow
//    the pointer to find the right version.
// 4. if rtti is available, vtable also holds type_info
// 5. subclass wil extened the layout of it's parent class.

struct complex {
  float real;
  float img;
  float abs() const { return std::hypot(real, img); }
};

// is

struct complex_compiled {
  float real;
  float img;
};
float abs(complex *self) { return std::hypot(self->real, self->img); }
