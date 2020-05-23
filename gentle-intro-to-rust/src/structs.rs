pub fn tuples() {
    fn add_mul(x: f64, y: f64) -> (f64, f64) {
        (x + y, x * y)
    }

    let t = add_mul(1.0, 2.0);
    let (a, b) = add_mul(3.0, 4.0);

    println!("{}, {}", t.0, t.1);
    println!("{}, {}", a, b);

    for t in ["zero", "one", "two"].iter().enumerate() {
        println!("idx: {}, value: {}", t.0, t.1);
    }

    for t in ["a", "b", "c"].iter().zip([1, 2, 3].iter()) {
        println!("name: {}, value: {}", t.0, t.1);
    }
}

#[derive(Debug)]
struct Person {
    first: String,
    second: String,
}

impl Person {
    // static method.
    fn new(first: &str, second: &str) -> Person {
        Person {
            first: first.to_string(),
            second: second.to_string(),
        }
    }

    // access data but not modify it.
    fn full_name(&self) -> String {
        format!("{} . {}", self.first, self.second)
    }

    fn copy(&self) -> Self {
        Self::new(&self.first, &self.second)
    }

    // access data and modify it.
    fn set_first(&mut self, name: &str) {
        self.first = name.to_string();
    }

    // deconstruct.
    fn to_tuple(self) -> (String, String) {
        (self.first, self.second)
    }
}

pub fn structs() {
    // like an interface.

    let p = Person {
        first: "Jane".to_string(),
        second: "John".to_string(),
    };

    let p2 = Person::new("Jimmy", "Yao");

    println!("Person {} {}", p.first, p.second);
    println!("Person {} {}", p2.first, p2.second);
    println!("Full name: {} ", p2.full_name());
    println!("Copied name: {}", p2.copy().full_name());
    let mut p3 = p2.copy();
    p3.set_first("Jummy");

    println!("Set name: {}", p3.full_name());

    // this will move the value, p3 now is destructed.
    let (first, second) = p3.to_tuple();
    println!("To tuple {} {}", first, second);

    println!("print debug p {:?}", p);
}

#[derive(Debug)]
struct A {
    // static lifetime, which exists through the whole program.
    s: &'static str,
}

#[derive(Debug)]
struct B<'a> {
    // 'a: the lifetime of struct itself.
    s: &'a str,
}

// the claim will never happen.
// fn makes_B() -> B<'static> {

//   let string = "I am a string".to_string();
//   A { s: &string }
// }

pub fn lifetime() {
    // all references to be stored must have a lifetime.
    // references cannot outlive owner.

    let a = A { s: "Hello" };
    println!("{}", a.s);
}
