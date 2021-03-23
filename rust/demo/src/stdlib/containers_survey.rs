use std::vec;

/// vector
pub fn vector_demo() {
    let _v: Vec<i32> = Vec::new();
    let mut v: Vec<i32> = vec![1, 2, 3];
    v.push(3);
    let i = v.pop().map(move |x| x + 1).unwrap();
    assert!(i == 4);
}

/// option
pub fn divide(numerator: f64, denominator: f64) -> Option<f64> {
    if denominator == 0.0 {
        None
    } else {
        Some(numerator / denominator)
    }
}

pub fn try_divide(a: f64, b: f64) -> Option<bool> {
    let result = divide(a, b);
    Some(match result {
        Some(_) => true,
        None => false,
    })
}
