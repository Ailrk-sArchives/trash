use memmap::Mmap;
use std::fs::File;
use std::io::{BufRead, BufReader, Error, Write};

fn read_lines_of_strings_from_file() -> Result<(), Error> {
    let path = "lines.txt";
    let mut output = File::create(path)?;
    write!(output, "Hihi tostitos\n")?;
    write!(output, "Add another line\n")?;
    write!(output, "is never a problem\n")?;
    write!(output, "yohoho")?;

    let input = File::open(path)?;
    let buffered = BufReader::new(input);

    buffered
        .lines()
        .for_each(|line| println!("{}", line.unwrap()));

    Ok(())
}

fn memory_map() -> Result<(), Error> {
    let file = File::open("content.txt")?;
    let map = unsafe { Mmap::map(&file)? };

    let random_indexes = [0, 1, 2, 20, 10, 11, 34];
    assert_eq!(&map[3..13], b"minecraft");
    let random_bytes: Vec<u8> = random_indexes.iter().map(|&idx| map[idx]).collect();
    assert_eq!(&random_bytes[..], b"My loaf!");
    Ok(())
}
