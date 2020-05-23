use std::f64::consts;

pub fn hello_world() {
    /* basics of rusts */

    // print is a macro
    print!("hello");

    // val are immutable by default.
    let value = 42;

    // assert is a macro
    assert_eq!(value, 42);
}

pub fn looping_if() {
    // for in range
    for i in 0..5 {
        if i % 2 == 0 {
            println!("even {}", i);
        } else {
            println!("odd {}", i);
        }
    }

    for i in 0..5 {
        // if as expression
        let even_odd = if i % 2 == 0 { "even" } else { "odd" };
        print!("{} {}", even_odd, i);
    }
}

pub fn adding_thigns_up() {
    // delare mutable variable.
    let mut sum = 0.0;
    for i in 0..5 {
        sum += i as f64;
    }

    print!("sum is {}", sum);
}

pub fn function_types() {
    fn sqr(x: f64) -> f64 {
        x * x
    };

    fn abs(x: f64) -> f64 {
        if x > 0.0 {
            x
        } else {
            -x
        }
    }

    fn clamp(x: f64, start: f64, end: f64) -> f64 {
        if x < start {
            start
        } else if x > end {
            end
        } else {
            x
        }
    }

    fn factorial(n: u64) -> u64 {
        if n == 0 {
            1
        } else {
            n * factorial(n - 1)
        }
    }

    fn by_ref(x: &i32) -> i32 {
        *x + 1
    }

    fn modifies(x: &mut f64) {
        *x = 10.0;
    }

    let res = sqr(clamp(abs(-0.9), 0.1, 1.4));
    println!("square is {}", res);
    println!("factorial is {}", factorial(10));

    let i = 10;
    let res1 = by_ref(&i);
    let res2 = by_ref(&31);
    println!("{} {}", res1, res2);

    let mut j = 1.0;
    modifies(&mut j as &mut f64);
    print!("{} ", j);
}

pub fn find_the_ropes() {
    fn pi1() {
        let pi: f64 = 3.1415;
        let x = pi / 2.0;

        let cosine = x.cos();
        print!("cosine {}", cosine);
    }

    fn pi2() {
        let x = 2.0 * consts::PI;
        let abs_difference = (x.cos() - 1.0).abs();

        assert!(abs_difference < 1e-10);
    }

    pi1();
    pi2();
}

pub fn array_and_slices() {
    let arr: [i32; 4] = [10, 20, 30, 40];
    let first = arr[0];
    let res = sum(&arr);

    // pass array by ref
    fn sum(values: &[i32]) -> i32 {
        let mut acc = 0;
        for i in 0..values.len() {
            acc += values[i];
        }
        acc
    }

    println!("first {}", first);
    for i in 0..arr.len() {
        println!("[{}] = {}", i, arr[i]);
    }
    print!("length {}", arr.len());

    println!("sum {}", res);

    // slice
    let ints = [1, 2, 3];
    let floats = [1.1, 2.2, 3.3];
    let strings = ["hello", "world"];
    let ints_ints = [[1, 2], [3, 4]];

    // rust slice borrow data rather than copying.
    let slice1 = &ints[0..2];
    let slice2 = &ints[1..];

    println!("ints {:?}", ints);
    println!("floats {:?}", floats);
    println!("strings {:?}", strings);
    println!("ints_ints {:?}", ints_ints);
    println!("slice1 {:?}", slice1);
    println!("slice2 {:?}", slice2);
}
