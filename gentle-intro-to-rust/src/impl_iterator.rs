use std::fmt;

pub struct PRange {
    start: f64,
    end: f64,
    step: f64,
}

pub fn range(start: f64, end: f64, step: f64) -> PRange {
    return PRange { start, end, step };
}

impl fmt::Debug for PRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} | {} | {}", self.start, self.end, self.step)
    }
}

// typeclass
impl Iterator for PRange {
    type Item = f64;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.start;
        if res > self.end {
            None
        } else {
            self.start += self.step;
            Some(res)
        }
    }
}

fn dump<T>(value: &T)
where
    T: fmt::Debug,
{
    // called trait bound.
    // just type class constraint.
    println!("from dumps: {:?} ", value);
}

pub fn rust_filter() {
    let r = range(0.0, 10.0, 1.0);
    dump(&r);
    let v: Vec<f64> = r.map(|x| x.sin()).collect();
    println!("{:?}", v);
}

pub fn sqrt<T>(x: T) -> T::Output
where
    T: std::ops::Mul + Copy,
{
    x * x
}
