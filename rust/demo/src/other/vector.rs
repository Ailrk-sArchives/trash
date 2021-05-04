use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;

// implement vector to get a feel of the overall flow.

struct Unique<T> {
    ptr: *const T,
    _marker: PhantomData<T>,
}

unsafe impl<T: Send> Send for Unique<T> {}
unsafe impl<T: Sync> Sync for Unique<T> {}

impl<T> Unique<T> {
    pub fn new(ptr: *mut T) -> Self {
        Unique {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> *mut T {
        self.ptr as *mut T
    }
}

pub struct Vector<T> {
    ptr: Unique<T>,
    cap: usize,
    len: usize,
}
