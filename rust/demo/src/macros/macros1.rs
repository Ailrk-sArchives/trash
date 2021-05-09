mod macros1 {

    macro_rules! four {
        () => {
            1 + 3
        };
    }

    macro_rules! gibberish {
        (4 fn ['spang "whoammo"] @_@) => {...};
    }

    // different kinds of captures
    // - item: (function, sturct, module, etc.)
    // - block: a block of statement/expression
    // - stmt: a statement
    // - pat: a pattern
    // - expr: an expression
    // - ty: a type
    // - ident: an indentifier
    // - path: a path (::std::mem::replace)
    // - meta: a meta item in #[...] or #![...]
    // - tt: single token tree.

    macro_rules! one_expression {
        ( $e:expr ) => {...};
    }

    macro_rules! times_five {
        ( $e:expr ) => {
            5 * $e
        };
    }

    macro_rules! multiple_add {
        ( $a:expr, $b:expr, $c:expr ) => {
            $a + ($b + $c)
        };
    }

    macro_rules! vec_strs {
        ( $( $e:expr),* ) => {
            {
                let mut v = Vec::new();
                $(v.push(format!("{}", $e));)*
                v
            }
        }
    }

    #[cfg(test)]
    mod test {
        #[test]
        fn test1() {
            let a = 1 + four!();
            let b = times_five!(a);
            let c = multiple_add!(a, b, 10);
            println!("{:?}", c);
        }

        #[test]
        fn test2() {
            let vecs = vec_strs![1, 2, "asd", 'a'];
        }
    }
}
