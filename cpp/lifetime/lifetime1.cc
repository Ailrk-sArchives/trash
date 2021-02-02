#include <iostream>

// each object and reference has their lifetime.
// lifetime is a runtime property, there is a moment
// that a program begins, and a moment that it ends.

// The lifetime of an obejct begins when
// 1. storage with proper alignment is obtained and
// 2. initialization is complete.

// The lifetime of an object ends when
// 1. if it's a non class type, it's destoryed.
// 2. if it's a class type, it's destructor call starts
// 3. the storage which the object occupies is released,
//    or is reused by an object that is not nested within
//    it.

// In c++ you have different storage durations
// 1. auto (automatic storage duration)
// 2. register (automatic storage duration + hint to place object in
//              processor's register)
//
// 3. static (static storage duration, internal linkage)
// 4. extern (static storage duration, external linkage)
// 5. thread_local (thread storage duration)
//
// life time of an object is equal to or is nested within the
// lifetime of it's storage.
// So if a value is allocated on the stack, it has the automatic
// duration that is the same as the scope. Thus it will not live
// longer than the scope.

// Lifetime of reference begins when its initialzation is complete and ends
// as if it's a object.

// Note: the life time of referred object might end before end of lifetime
// of the reference, which make dangling references possible.
