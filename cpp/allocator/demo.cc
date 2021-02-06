#include <iostream>

// Why do you want a custome allocator?
// - separate allocastion from construction
// - separate destruction from deallocastion

// The purpose of allocator
// - Encapsulate information of allocation strategy
// - Encapsulate informatoin about addressing model
// - Hide memory management and addressing model details
// - Support reuse

// Types of Alocators?
//
// +--------------------+-------
// |         |  global  |  local
// +---------+----------+-------
// | general | jemalloc |  multipool_allocator
// +---------+----------+-------
// | special |  malloc  |  alloca
// +---------+----------+-------
//
// 1. Local allocator can be beneficial from it's
//    compact memeory. You have better locality to
//    exploit cache lines.
//    (Locality might be the most important property for
//     modern hardware in terms of performance)
//
// 2. Global general allocators also consider thread safety
//    But for single threaded application you might not
//    care about that.
//
// 3. alloca alocate space on the stack. It's very good
//    when you can save heap allocation, but it also
//    make stack overflow very easy.
//
// 4. Local alloator has the notion of release. You can just
//    forget about the memory without deallocate.
//    0 deallocation time.

// Possible allocator interface
// 1. as a function (malloc/free)
// 2. as reference wrapper template parameter.
// 3. pass the address of pure abstract base class.

// Allocator decision tree
//            Start
//    no -- Should I use an allocator?
//             |
//             | yes
//             |
//          Via Base class?
//             | (both yes and no)
//             |
//             |
//          Which allocator?
//          |     |     | ...
//          A     B     C
//          |     |     |
//          Wink out memory ?
//             |
//          Use optimal local allocation strategy

// monotonic, multipool, or multipool<monotonic>

// Five dimensions of memory allocation
// 1. Density of allocation
// 2. Varaition of allocated size
// 3. Locality of access memory
// 4. Utilization of allocated memory
// 5. Contention of oncurrent allocations.

// For performance, the goal of allocation is not to
// allocate things fast, but rather allocate things
// to achieve better locality.

// Some example of allocators
// - Stack based allocation
// - Per-container private allocation
// - per-thread allocation
// - pool/slab allocation
// - Areana allocation.
// - Debug
// - Relocatable data.

// Allocator propagations.
// - Lateral propagation
//  - copy/move construct
//  - copy/move assignment
//  - swap
// - Deep propagation.
//  - for case like, map<string, vector<list<string>>>,
//    you want to pass the same allocator down to all
//    other containers so they can share the same arena.
//  - scoped_allocater_adaptor

int main(void)
{

  return 0;
}

