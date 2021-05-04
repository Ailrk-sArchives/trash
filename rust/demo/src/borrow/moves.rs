// simple moves

struct AType;

impl Drop for AType {
    fn drop(&mut self) {
        println!("Droping A type");
    }
}

fn foo_move<T>(x: T) -> Option<T> {
    println!("foo_move: ");
    None
}

fn foo_borrow<T>(x: &T) -> Option<T> {
    println!("foo_borrow: ");
    None
}

fn foo_borrow_mut<T>(x: &mut T) -> Option<T> {
    println!("foo_borrow_mut: ");
    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn drop_atype() {
        let _ = {
            let a = AType {};
            a
        };
    }

    #[test]
    fn basics() {
        let a = AType {};

        foo_move(a);

        // this doesn't work any more.
        // a is moved
        // foo_move(a);

        let a = AType {};

        // shared reference. Have has many as you like.
        foo_borrow(&a);
        foo_borrow(&a);
        foo_borrow(&a);
        foo_borrow(&a);

    }
}
