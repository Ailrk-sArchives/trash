pub fn largest<T>(list: &[T]) -> T
where
    T: PartialOrd + Copy,
{
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

pub struct Point<T> {
    x: T,
    y: T,
}

// here it return a reference.
// If the return type is T, then this method
// will hand over the ownership of x, which is
// not desirable.
// The ownershipo things is also reverlent when making
// libraries.
impl<T> Point<T> {
    pub fn x(&self) -> &T {
        &self.x
    }
}

pub struct HeterPoint<T, U> {
    x: T,
    y: U,
}

impl<T, U> HeterPoint<T, U> {
    pub fn mixup<V, W>(self, other: HeterPoint<V, W>) -> HeterPoint<T, W> {
        HeterPoint {
            x: self.x,
            y: other.y,
        }
    }
}

pub fn use_point() {
    let hp = HeterPoint::<i32, i32> {
        // turbo fish to specify type.
        x: 10,
        y: 10,
    };
    let p = Point::<i32> { x: 10, y: 20 };

    println!("{:?}", p.x());
    let hp2 = hp.mixup(HeterPoint::<u32, u32> { x: 100, y: 100 });

    println!("{:?}", hp2.x);
}
