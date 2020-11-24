// Contribution:
//  1. Specificatoin language for expressing functional properties of rust.
//  2. Using Pledges (novel specification construct that enables modular specification)
//  3. defined a verification technique encodes both capability information and user provided
//     assertion into `implicit dynamic frane logic`
//     (closely related to separatoin logic https://www.cs.cmu.edu/~jcr/seplogic.pdf)
//  4. The verification is translated from rust into viper itermediate verification language.
//  5. The technique is provided as a plugin for rust compiler.


// What properties need to be ensured so we can call this program correct?
// 1. shift_x increase p1.x by diff.    -- what need to be verified.
// 2. call doesn't modify p2.x
// 3. shift_x doesn't modify tuple segm.
// 4. data race free, the values of all memory locations are stable throughtout
//    the execution. (With the borrow rule, we don't need to verify special
//                    case in multi-threading)
//
// Now, verification will prove these properties in general (for rust)
// (Capabiliaty information) + (Statement semantics)
// 1. capability of memory location (mut ref or shared ref)
//  1. aliasing
//  2. side effect
//  3. framing
//  4. data race
// 2. information of values.
#[derive(Debug)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

pub fn shift_x(p: &mut Point, s: i32) {
    // take a mutable reference
    p.x = p.x + s;
}

// - Note: Box guarantee unique ownership of the boxed value.
//         so there will never be alias of other data.
// - Note: Since we can guarantee segm.0 and segm.1 are different points,
//         calling shift will guarantee segm.0 is chagned while segm.1 is
//         not touched.
pub fn align(mut segm: (Box<Point>, Box<Point>)) -> (Box<Point>, Box<Point>) {
    let diff = (*segm.1).x - (*segm.1).x;

    shift_x(&mut segm.1, diff); // borrows the ownership here.
                                // borrow expired after this line.

    // it's ok to use semg.2, since nothing is borrowing it now.
    assert!((*segm.1).x == (*segm.1).x);
    segm
}

pub fn run() {
    let seg = {
        let p1 = Point { x: 10, y: 20 };
        let p2 = Point { x: 100, y: 30 };

        let old = (Box::new(p1), Box::new(p2));
        println!("old: {:?}", old);
        old
    };
    let new = align(seg);
    println!("new: {:?}", new);
}

// Process of Verification
// Program + User specification --Concert--> Verification language
// --Verify--> Pass | Not pass
//
// What's different about prusti?
//  1. need to use separation logic for imperative language.
//      1. with verification condition generator
//          1. based on hoare logic.(relationship between precondition and postcondition).
//  2. rust has ownership (make things easier actually)
//  3. use viper's permission based on separation logic to express ownership and borrow.
//
// Liquid haskell
//  1. mainly solved with SMT solver
//  3.
//
//
// What's viper?
//  1. viper is a verification language. (theorem prover)
//
//
