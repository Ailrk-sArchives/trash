#include <iostream>
#include <memory>
#include <vector>


// you can implement your own  allocator
// as long as it compliant with the allocator traits,
// and the container will expect to use your allocator
// via the interface provided by the allocator trait.

// if there are things not implement from your own allocator,
// the allocator trait will pick a default.
