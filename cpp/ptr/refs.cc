#include <iostream>

int main(void) {

  int i, j, *p;

  // 7 is a prvalue, assigned to lvalue.
  i = 7;

  // 7 = i invalid. Left operant must be a lvalue.

  // dereferenced pointer is a lvalue.
  *p = i;

  // the expression return an lvalue.
  ((i < j) ? i : j) = 7;

  // you cannot bind temporary object to a
  // non const lvalue refernce

  return 0;
}

// lvalue reference &
// holds the address of an object but behaves syntatically like an object.
// char & name;
//
// lvalue reference is the actually reference we talk about normally.
// lvalue reference can be thought as an alias of an object.
//
// a reference must be initialized! and the reference cannot be changed.


// rvalue reference &&
// concepts related to rvalue references: move semantics, perfect forwarding.
//
// We say rvalue reference support move semantics. What does that mean is
// if you have a rvalue reference, it refers to a temporary value. By knowing
// that we can move it to any lvalue reference directly without copy it.
//
// To implement move semantics you implement the move constructor.
// The compiler will invoke the constructor when it's appropriate.
//
//



/////////////////////////////////////////////
// explanation of why c++ added reference. //
/////////////////////////////////////////////
// Why reference are good?
// https://www.quora.com/Why-were-references-introduced-to-C++-when-we-already-had-pointers
//
// Imagine you are in c. Here you made a struct A, and a function return the
// struct.
struct A {
  int a;
};

struct A f() {
  A a = {1};
  return a;
};
// Your compiler knows it needs to return the value by copy.
// and because it's just a simple struct, copy is easy, just
// memcpy the memory to the new location.

// But here, imagine you have a c++ class.

class Foo {
  int a;
};

Foo g() {
  Foo a;
  return a;
}

// Looks similar, but c++ has constructors.
//
// Even if you don't declare anything, g++ will still create
// default copy constructor and move constructor for you.
// So the empty class has at least two hidden methods.
//
// a get returned, compiler know we want to copy this value.
// And to copy it we need to call a copy constructor implicitly.
// The compiler now need to find a way to pass this object
// into the copy constructor.
//
// To do that, we can't pass by value, because that will invoke another
// copy...
//
// So how about a pointer though?
// If we pass a pointer instead, the meaning of current syntax will be different.
// We say we want to return by value, but we really returned a pointer to
// the copy constructor.
//
// If we introduce reference, which merely another name for a variable it's
// possible that while we return a by value, we return the reference of
// it without invoke the copy constructor.
//
//



// compiler created constructors.
//
// There are three types of constructors the compiler can create for us:
//  1. copy constructor       A(const A&)
//  2. move constructor       A(A &&)
//  3. default constructor    A()
//
// * The body of default constructor created is empty;
//    Rationale:
//      If you don't specify anything, it just works like a c struct.
//      The memory get allocated and non initialized.
//
// * The body of default copy constructor copy all data members of one object to another.
//    Rationale
//      That's what a copy means.
//      In c++ all copy constructors should mean actual deep copy.
//      Shallow copy in other languages is just copy pointers.
//
// * As long as you declare any other constructor, there will be no default constructor
//    Rationale:
//      If you already defined some costructors, for most of the time you want to take
//      over the control of object creation. In that sense a copy constructor will be
//      an extra trouble.
//
// * Compiler create default and copy constructor if they are not explicitly defined.
//    Rationale:
//       When nothing is defined, just consider how do you make it behaves as close
//       as possible to a C struct.
//       You can declare a struct without initialize it, default constructor takes care of that.
//       You can copy a struct, and you have default copy constructors.
//
// * Move constructors will be generated iff there is no user declared copy, move, and destructor.
//    Rationale:
//       Notice if you declared a copy constructor, there will be no move constructor declared for you.
