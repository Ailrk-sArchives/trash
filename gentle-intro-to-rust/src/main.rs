mod basic;
mod enums;
mod error;
mod files;
mod generics;
mod hashm;
mod hello;
mod impl_iterator;
mod matching;
mod other;
mod structs;
mod traits;
mod lifetime;
mod fib_iter;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    println!("args: {:?}", args);

    println!("pub fn hello_world():");
    hello::hello_world();

    println!("pub fn looping_if() ");
    hello::looping_if();

    println!("pub fn adding_thigns_Up()");
    hello::adding_thigns_up();

    println!("function_types");
    hello::function_types();

    println!("pub fn find_the_ropes()");
    hello::find_the_ropes();

    println!("pub fun array_and_slices()");
    hello::array_and_slices();

    println!("pub fn optional()");
    basic::optional();

    println!("pub fn vectors()");
    basic::vectors();

    println!("pub fn iterators()");
    basic::iterators();

    println!("pub fn strings()");
    basic::strings();

    println!("pub fn more_string()");
    basic::more_string();

    println!("pub fn matching()");
    matching::matching();

    println!("pub fn reading_files()");
    files::reading_files();

    println!("use result");
    files::use_result();

    println!("pub fn reading_files_safe()");
    files::reading_files_safe();

    println!("pub fn use_io_result_type()");
    files::use_io_result_type();

    println!("tuple()");
    structs::tuples();

    println!("strucst()");
    structs::structs();

    println!("lifetime()");
    structs::lifetime();

    println!("show_trait()");
    traits::show_trait();

    println!("type trait for iterator");
    println!("{:?}", impl_iterator::range(0.0, 10.0, 1.0).next());

    println!("rust_filter()");
    impl_iterator::rust_filter();

    println!("sqrt() generic");
    println!("{}", impl_iterator::sqrt(3.4));

    println!("ipaddress()");
    enums::ipaddress();

    enums::Message::Write(String::from("writing")).call();
    enums::Message::Quit.call();
    enums::Message::Move { x: 10, y: 10 }.call();

    other::restuarant::front::Tables::BigRound(String::from("no 1"));

    println!("Hello, world!");

    hashm::insert_m();
    hashm::insert_or();
    hashm::create_from_zip();

    error::no_file();
    error::closure_error_handling();
    error::use_question_mark();

    let l = generics::largest::<u16>(&vec![20, 30, 10, 320, 2, 3, 12, 1000]);
    println!("{}", &l); // has Copy trait. No move here.
    println!("{}", l);

    generics::use_point();

    let s: &str = lifetime::longest("abc", "bcdsd");
    println!("{}", s);

    lifetime::struct_lifetime();

    let s: &str = lifetime::first_word("first word");
    println!("{}", s);

    let s: &str = lifetime::longest_with_an_announcement("a", "asd", "good");
    println!("{}", s);

    let fib = fib_iter::Fibnacci::new();
    for (i, f) in fib.take(80).enumerate() {
        println!("fib {}-> {} or\n {:#b}\n or {:#x}\n", i, f, f, f);
        println!("==================================>");
    }
}
