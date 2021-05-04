// given this function
fn compute1(input: &i32, output: &mut i32) {
    if *input > 10 {
        *output = 1;
    }
    if *input > 5 {
        *output *= 2;
    }
}

// we can rewrite it as this in rust:
// we can do this because a mutable reference cannot be aliased.
// this ensure input and output are different references.
fn compute2(input: &i32, output: &mut i32) {
    let v = *input;
    if v > 10 {
        *output = 1;
    } else if v > 5 {
        *output *= 2;
    }
}

// but in C this will be unsafe. Because we can guarantee input and output
// are aliases of the same value.

// the key point is write is what makes aliasing problematic.
// if there is no write operation, we don't really care how many aliases
// are there.
//
// So we have mutable reference that can't have any aliases, and normal
// references that can have as many aliases as you want.
//
// Aliases in rust carries the notion of liveness and mutation.
