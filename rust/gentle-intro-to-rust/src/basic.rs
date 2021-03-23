pub fn optional() {
    let ints: Vec<i32> = (0..10).collect::<Vec<_>>();
    let slice = &ints;
    let first = slice.get(0);
    let last = slice.get(11);
    println!("first {} {}", first.is_some(), first.is_none());
    println!("last {} {}", last.is_some(), last.is_none());
    println!("fist value {}", *first.unwrap());
    let last_result = *slice.get(11).unwrap_or(&-1);
    println!("{}", last_result);
}

pub fn vectors() {
    let mut v = Vec::new();
    v.push(1);
    v.push(2);
    v.push(3);
    let first = v[0];
    let maybe_first = v.get(0);
    println!("v is {:?}", v);
    println!("first is {}", first);
    println!("maybe_first is {:?}", maybe_first);
    // use borrow operator.
    // the slice borrow the memory from vector.
    let slice = &v[1..];
    println!("slice is {:?}", slice);
}

pub fn iterators() {
    // like python iterator.
    let mut iter = 0..3;
    assert_eq!(iter.next(), Some(0));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), None);
    let arr = (0..5).collect::<Vec<_>>();
    for n in arr.iter() {
        println!("{}", n);
    }
    let sum: i32 = (0..5).sum();
    println!("sum was {}", sum);
    let sum: i64 = [10, 20, 30].iter().sum();
    println!("sum was {}", sum);
    // window of slice of arr.
    for s in (&arr).windows(2) {
        println!("window {:?}", s);
    }
    for s in arr.chunks(3) {
        println!("chunk {:?}", s);
    }
    let mut v1 = vec![10, 20, 30, 40];
    v1.pop();
    let mut v2 = Vec::new();
    v2.push(10);
    v2.push(20);
    v2.push(30);
    assert_eq!(v1, v2);
    v2.extend(0..2);
    assert_eq!(v2, &[10, 20, 30, 0, 1]);
    let mut v3 = vec![1, 10, 5, 1, 2, 11, 2, 540];
    v3.sort();
    v3.dedup(); // remove duplicates.
    println!("{:?}", v3);
}

pub fn strings() {
    // slice of String is str
    fn dump(s: &str) {
        println!("str '{}'", s);
    }

    fn convert_string_and_literal_str() {
        let text = "hello dolly"; // string slice.
        let s = text.to_string(); // now heap string.
        let mut string = String::new();
        string.push('H');
        string.push_str("ello");
        string.push(' ');
        string += "world"; // push_str
        string.pop();
        dump(text);
        dump(&s);
        dump(&string);
    }

    fn format() {
        // pass in a arry with i32.
        fn array_to_str(arr: &[i32]) -> String {
            // initialize a char into string.
            let mut res = '['.to_string();
            for v in arr {
                // iter through the slice.
                res += &v.to_string();
                res.push(',');
            }
            res.pop();
            res.push(']');
            res
        }
        let arr = array_to_str(&[10, 20, 30]);
        let res = format!("hello {}", arr);
        println!("{}", res);
    }
    convert_string_and_literal_str();
    format();
}

pub fn more_string() {
    fn slice_on_string() {
        // a literal str
        let text = "static";
        // convert into string.
        let string = "dynamic".to_string();
        // slice a str
        let text_s = &text[1..];
        // slice a string
        let strings = &string[2..4];
        println!("slices {:?} {:?}", text_s, strings);
    }

    fn string_encodeing_form() {
        let chinese = "一行中文";
        // chars to return a char iterator of str.
        for ch in chinese.chars() {
            println!("{}", ch);
        }
        println!("");
        println!("len {}", chinese.len()); // len of byte.
        println!("count {}", chinese.chars().count());
        // find will convert character position to byte position.
        let maybe = chinese.find('行');
        if maybe.is_some() {
            // use character to slice.
            let h = &chinese[maybe.unwrap()..];
            println!("Chinese {}", h);
        }
    }

    fn string_slice_and_chars_slice() {
        // let s = "个";  // these will be a runtime error.
        let s = "i";
        assert_eq!(s, "i");
        println!("{}", &s[0..1]);
    }

    fn split() {
        let text = "the quick fox jumps over the lazy dog";
        let words: Vec<&str> = text.split(' ').collect();
        println!("{:?}", words);
        let stripped: String = text.chars().filter(|ch| !ch.is_whitespace()).collect();
        print!("{:?}", stripped);
        let mut stripped: String = String::new();
        for ch in text.chars() {
            if !ch.is_whitespace() {
                stripped.push(ch);
            }
        }
    };

    slice_on_string();
    string_encodeing_form();
    string_slice_and_chars_slice();
    split();
}
