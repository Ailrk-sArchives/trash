use std::mem;


// Rust doesn't allow you to move out a value that implement Drop.

#[derive(Debug)]
struct A;

#[derive(Debug)]
struct B;

#[derive(Debug)]
struct Foo {
    a: A,
    b: B,
}

impl Foo {
    fn take(self) -> (A, B) {
        (self.a, self.b)
    }
}

#[derive(Debug)]
struct Boo {
    a: A,
    b: B,
}

impl Drop for Boo {
    fn drop(&mut self) {}
}

// because we implemened drop foo, so when take()
// returns, self is out of scope.
// self.drop will be called.
// but since self's fields a, b are moved out,
// the original fields will be dangling reference, and
// dropping on it will be UB.

macro_rules! assume_init {
    () => {{
        mem::MaybeUninit::uninit().assume_init()
    }};
}

// here we replcae old fields with dummy value, then just forget self all together.
// forget means destruct the value with
impl Boo {
    fn take(mut self) -> (A, B) {
        let a = mem::replace(&mut self.a, unsafe { assume_init!() });
        let b = mem::replace(&mut self.b, unsafe { assume_init!() });
        mem::forget(self);
        (a, b)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() {
        let foo = Foo { a: A, b: B };
        let (a, b) = foo.take();
        println!("{:?}", a);
        println!("{:?}", b);
    }
}
