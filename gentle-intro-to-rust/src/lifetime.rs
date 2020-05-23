use std::fmt::Display;

pub fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}


struct ImportantExcerpt<'a> {
    // instance of ImportantExcerpt cannot outlive part.
    part: &'a str,
}

pub fn struct_lifetime() {
    let novel = String::from("McCafe. is better than Timhortons");
    let first_sentence = novel.split('.')
        .next()
        .expect("Could not find a '.'");

    let i = ImportantExcerpt { part: first_sentence };
    println!("struct with a reference {}", i.part);

}


// an example that doesn't need a lifetime annotation.
// lifetime elision rules.
pub fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }
    &s[..]
}

// lifetime_mixture
pub fn longest_with_an_announcement
    <'a, T>(x: &'a str, y: &'a str, ann: T) -> &'a str
    where T: Display
{
    println!("Announcement! {}", ann);

    if x.len() > y.len() {
        x
    } else {
        y
    }

}
