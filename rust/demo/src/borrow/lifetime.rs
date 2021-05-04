// normally you write your code like this.
// but it's actually a sugar. it really looks like this:
// Every thing after a statement will have a new lifetime.
fn lifetime1() {
    let x = 0;
    let y = &x;
    let z = &y;
}

fn lifetime2() {
    // 'a
    let x = 0;
    {
        // 'b
        let y = &x;
        {
            // 'c
            let z = &y;
        }
    }
}

// an example of return dangling pointer
fn as_str(data: &u32) -> String {
    format!("{}", data)
}

fn static_str(data: &u32, out: &mut String) {
    let s = format!("{}", data);
    *out = s;
}

// aliasing a mutable reference

fn aliasing_a_mutable_reference() {
    let mut data = vec![1, 2, 3];
    let x = &data[0]; // immutable borrow of data
    data.push(4); // mutable data

    // using immutable data here.
    // this is not allowed as mutable and immutable references are
    // in the same lifetime.
    // println!("{}", x);
}

fn aliasing_a_mutable_reference_revealed() {
    let mut data = vec![1, 2, 3];
    {
        let x = &data[0]; // immutable borrow of data
        {
            data.push(4); // mutable data
        }
        // data.push creates a mutable reference.
        // x lives longer than data.push,
        // which means we have a mutable reference and an immutable
        // reference at the same time.
        // which fires a lifetime error.

        // println!("{}", x);
    }
}

#[derive(Debug)]
struct X<'a>(&'a i32);

impl Drop for X<'_> {
    fn drop(&mut self) {}
}

fn drop_causes_problem() {
    let mut data = vec![1, 2, 3];
    let x = X(&data[0]);
    println!("{:?}", x);

    // we need to drop x before data.push.
    // because Drop also takes a reference.
    drop(x);
    data.push(4);
}

// limitation of life time.
// TODO
