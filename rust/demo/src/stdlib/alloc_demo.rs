use std::alloc::{GlobalAlloc, Layout, System};

// global memory allocator.

struct MyAllocator;

unsafe impl GlobalAlloc for MyAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        System.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
    }
}

// configure the choice of global allocator. We can use this allocator
// for all types.
#[global_allocator]
static GLOBAL: MyAllocator = MyAllocator;

pub fn run () {
    let mut v = Vec::new();
    v.push(1);
}
