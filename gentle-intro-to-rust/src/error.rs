use std::fs::File;
use std::io;
use std::io::ErrorKind;
use std::io::Read;

pub fn no_file() {
    let name: &str = "not_a_file.txt";
    let f = File::open(name);
    let mut buf = String::new(); // initialize the string first.

    let mut f = match f {
        // use match to handle Results.
        Ok(file) => file,
        // nested match for different errors.
        Err(error) => match error.kind() {
            // here can use a if let.

            // different error types.
            ErrorKind::NotFound => match File::create(name) {
                Ok(fc) => fc,
                Err(e) => panic!("Can not create file {:?}", e),
            },

            other_err => panic!("Can not create file {:?}", other_err),
        },
    };

    // if let discard the fail case.
    if let Ok(_) = f.read_to_string(&mut buf) {
        println!("{}", buf);
    };
}

pub fn closure_error_handling() -> Result<String, io::Error> {
    // use closure to handle error instead of match
    // This make code more concise.
    let filename: &str = "not_a_file_2.txt";
    let mut buf = String::new();
    let mut f = File::open(filename).unwrap_or_else(|e| {
        if e.kind() == ErrorKind::NotFound {
            File::create(filename).unwrap_or_else(|e| {
                panic!("Problem creating the file {:?}", e);
            })
        } else {
            panic!("Problem creating the file {:?}", e);
        }
    });

    match f.read_to_string(&mut buf) {
        Ok(_) => Ok(buf),
        Err(e) => Err(e),
    }
}

pub fn use_question_mark() -> Result<String, io::Error> {
    let mut buf = String::new();
    let filename = "not_a_file_3.txt";

    // the quetion mark gonna return the error automatically.
    File::open(filename)?.read_to_string(&mut buf)?;
    Ok(buf)
}
