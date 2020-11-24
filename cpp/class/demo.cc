#include <iostream>

// c++ data models with constructors and destructors.
// The idea of having class is to make it easier to model real world problem.
// But when they need to run on the computer, how to efficiently manage their
// resources is also a problem we need to care about.
//
// c++ address this problem with RAII, you create an object by calling a constructor,
// and delete the object by calling it's destructor.
// Each object has their lifetime, within the lifetime the object will obtain resources,
// do it's work, free resources, and everything get managed properly.
//
// Constructor is nothing special but a function. By calling it you bring a new object
// into life with resources they require.
//
// What're resources? Say your class has two integer fields, so it will need to allocate
// 16 bytes to store those integers. The 16 memory space is an example of resource you
// acquire.
//
// The object can both be create on the heap or on the stack, but they will follow the same
// memory layout, and be created with the same procedure specified by the constructor.
//
// Among all the constructors you can define, there are two special constructors, which
// will be called by the compiler when their corresponding semantics get triggered.
//
// One is copy constructor, the other is move constructor.
// The common pattern is you pass another object (refernce, no matter lval ref or rval ref, they
// always ref to another obect with the same type) into the constructor, and the constructor
// decides to do something with the other object to create itself.
//
// Normally you get another object, you might want to copy it. You want to have another copy of
// the exact same memory layout, socket refs, etc as the object get passed in. Or maybe sometimes
// you don't want to copy the entire object but only part of it. These are all things you can
// specify in a copy construtor.
//
// What if you want to take over the object get passed in? Then you need the move semantics,
// steal resources from the other obejct directly.


// Notice for unique_ptr, it essentially still a object on the stack holds a poiner. Because it
// is a compile time construct, we can optimize it out and make it as efficient as a raw pointer.
// Of course copy an unique_ptr is not allowed. So what does it means to copy a unique_ptr? If you
// copy the unique_ptr, you probably want to copy the member field from one unique_ptr to another.
// And that make the object get pointed to by the member field an alias, no longer unique anymore.
//
// Ok now what does it means to move a constructor?

int main(void)
{

  return 0;
}
