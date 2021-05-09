// macros doesn't support backtracking, so you
// need to list macthing rules from the most specific
// to the least

mod macros2 {

    macro_rules! capture_expr_then_stringify {
        ( $e:expr ) => {
            stringify!($e)
        };
    }

    macro_rules! capture_then_match_tokens {
        ( $e:expr ) => {
            match_tokens!($e);
        };
    }

    macro_rules! match_tokens {
        ($a:tt + $b:tt) => {
            "Got an addition"
        };
        (($i:ident)) => {
            "Got an identifier"
        };
        ($($other:tt)*) => {
            "Got something else"
        };
    }

    macro_rules! what_is {
        (#[no_mangle]) => {"no_mangle attribute"};
        (#[inline]) => {"inline attribute"};
        ($($tts:tt)*) => {concat!("Something else (", stringify!($($tts)*), ")")};
        // tt can be used to capture anthing else.
    }

    macro_rules! capture_then_what_is {
        (#[$m:meta]) => {
            what_is!(#[$m])
        };
    }

    macro_rules! using_a {
        ($a:ident, $e:expr) => {{
            let $a = 42;
            $e
        }};
    }

    #[cfg(test)]
    mod test {

        #[test]
        fn test1() {
            // shows stringified token tree.
            println!("{:?}", stringify!(dummy(2 * (1 + (3)))));

            // shows stringified AST nodes.
            println!("{:?}", capture_expr_then_stringify!(dummy(2 * (1 + (3)))));
        }

        #[test]
        fn test2() {
            println!(
                "{}\n{}\n{}\n",
                match_tokens!((caravan)),
                match_tokens!(3 + 6),
                match_tokens!(5)
            );

            // alwasy got something else.
            println!(
                "{}\n{}\n{}\n",
                capture_then_match_tokens!((caravan)),
                capture_then_match_tokens!(3 + 6),
                capture_then_match_tokens!(5)
            );
        }

        #[test]
        fn test3() {
            println!(
                "{}\n{}\n{}\n{}",
                what_is!(#[no_mangle]),
                what_is!(#[inline]),
                capture_then_what_is!(#[no_mangle]),
                capture_then_what_is!(#[inline]),
            );
        }

        #[test]
        fn test4() {
            using_a!(a, a / 10);
        }
    }
}
