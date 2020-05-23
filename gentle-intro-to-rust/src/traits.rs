use std::fmt;

trait Show {
    fn show(&self) -> String;
}

// implement type trait.
// like interface, or typeclass.
impl Show for i32 {
    fn show(&self) -> String {
        format!("four-byte signed {}", self)
    }
}

impl Show for f64 {
    fn show(&self) -> String {
        format!("eight-byte signed {}", self)
    }
}

pub fn show_trait() {
    let i = 53;
    let f = 2.71;
    let i_show = i.show();
    let f_show = f.show();

    println!("values: {} {}", i, f);
    println!("show: {} {}", i_show, f_show);
}

struct Person<'a> {
    first: &'a str,
    second: &'a str,
}

// implement debug type trait for Person.
impl fmt::Debug for Person<'static> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format!("{} - {}", self.first, self.second))
    }
}
