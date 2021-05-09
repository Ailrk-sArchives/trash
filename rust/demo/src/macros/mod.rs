pub mod macros1;
pub mod macros2;

// rust macro expand after all code transformed into  AST.
// so any macro written must be valid rust syntax in the first place.

// token tree: [..], {..}, (..) creates a sub tree.

// syntax extension form we can expand.
// $name ! $args

mod examples1 {
    use std::collections::HashMap;

    // code we pass in will be initialized only when they are used.
    lazy_static! {
        static ref HASHMAP: HashMap<u32, &'static str> = {
            let mut m = HashMap::new();
            m.insert(0, "foo");
            m.insert(1, "foo");
            m.insert(2, "foo");
            m
        };
        static ref COUNT: usize = HASHMAP.len();
        static ref NUMBER: u32 = times_two(21);
    }

    fn times_two(n: u32) -> u32 {
        n * 2
    }

    fn run() {
        println!("The map has {} entries", *COUNT);
        println!("The entry for 0 is {}", HASHMAP.get(&0).unwrap());
        println!("A expensive calculatio on a static result in: {}.", *NUMBER);
    }
}

mod examples2 {

    // another example of using macros.
    bitflags! {
        struct Flags: u32 {
            const A = 0b00000001;
            const B = 0b00000010;
            const C = 0b00000100;
            const ABC = Self::A.bits | Self::B.bits | Self::C.bits;
        }
    }

    fn run() {
        let e1 = Flags::A | Flags::C;
        let e2 = Flags::B | Flags::C;
        assert_eq!((e1 | e2), Flags::ABC);
        assert_eq!((e1 & e2), Flags::C);
        assert_eq!((e1 - e2), Flags::A);
        assert_eq!(!e2, Flags::A);
    }
}

mod examples3 {
    bitflags! {
        struct Color: u8 {
            const RED = 0b0001;
            const GREEN = 0b0010;
            const BLUE = 0b0100;
            const BRIGHT = 0b1000;
        }
    }

    lazy_static! {
        static ref FIB_100: u32 = {
            fn fib(a: u32) -> u32 {
                match a {
                    0 => 0,
                    1 => 1,
                    a => fib(a - 1) + fib(a - 2),
                }
            }
            fib(100)
        };
    }

    fn run() {
        let colors = vec![Color::RED, Color::GREEN, Color::BLUE];
        println!("Helo, world");
    }
}
