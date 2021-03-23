pub fn matching() {
    let string = "the quick fox jumps over the lazy dog";
    match string.find('l') {
        Some(idx) => {
            let lazydog = &string[idx..];
            println!("{}", lazydog);
        }
        None => println!("Cannot find lazy dog"),
    };
    // ignore None
    if let Some(idx) = string.find('l') {
        println!("againg, {}", &string[idx..]);
    }
    // nested and switch
    let text = match string.find("l") {
        Some(idx) => match idx {
            0 => "ZERO",
            1 => "ONE",
            2 => "TWO",
            3 => "THREE",
            _ => "MANY ...",
        },
        None => "NONE...",
    };
    println!("{}", text);
}
