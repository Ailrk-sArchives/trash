use std::env;
use std::fs::File;
use std::io;
use std::io::Read;

pub fn reading_files() {
    // read file without handling exceptions.
    let first = env::args().nth(1).expect("please supply a file name");
    let mut file = File::open(&first).expect("can't open the file");
    let mut text = String::new();
    file.read_to_string(&mut text).expect("can't read the file");
    println!("file had {} bytes", text.len());
}

pub fn use_result() {
    // result is like either.
    fn good_or_bad(good: bool) -> Result<i32, String> {
        if good {
            Ok(42)
        } else {
            Err("bad".to_string())
        }
    }
    println!("{:?}", good_or_bad(true));
    println!("{:?}", good_or_bad(false));
    match good_or_bad(true) {
        Ok(n) => println!("Ok, it is {}", n),
        Err(e) => println!("Err {}", e),
    }
}

pub fn reading_files_safe() {
    /* use result to handle error cases */
    fn read_to_string(filename: &str) -> Result<String, io::Error> {
        let mut file = match File::open(&filename) {
            Ok(f) => f,
            Err(e) => return Err(e), // early return.
        };
        let mut text = String::new();
        match file.read_to_string(&mut text) {
            Ok(_) => Ok(text),
            Err(e) => Err(e),
        }
    }
    let file = env::args().nth(1).expect("please supply a filename");
    let text = read_to_string(&file).expect("bad file");
    println!("file content {}", text);
}

pub fn use_io_result_type() {
    fn read_to_string(filename: &str) -> io::Result<String> {
        let mut file = File::open(&filename)?; // early return for error.
        let mut text = String::new();
        file.read_to_string(&mut text)?;
        // should not have ; at the end.
        Ok(text)
    }
    let file = env::args().nth(1).expect("please supply a filename");
    let text = read_to_string(&file).expect("bad file");
    println!("file content: {}", text);
}
