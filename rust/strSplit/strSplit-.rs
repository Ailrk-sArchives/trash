#[derive(Debug)]
pub struct StrStrip<'a, T> {
    remainder: Option<&'a str>,
    delimiter: T,
}

impl<'a, T> StrStrip<'a, T> {
    pub fn new(haystack: &'a str, delimiter: T) -> Self {
        Self {
            remainder: Some(haystack),
            delimiter,
        }
    }
}

pub trait Delimiter {
    fn find_next(&self, s: &str) -> Option<(usize, usize)>;
}

impl<'a, T: Delimiter> Iterator for StrStrip<'a, T> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let remainder = self.remainder.as_mut()?;
        if let Some((del_start, del_end)) = self.delimiter.find_next(remainder) {
            let until_delimiter = &remainder[..del_start];
            *remainder = &remainder[del_end..];
            Some(until_delimiter)
        } else {
            self.remainder.take()
        }
    }
}

impl Delimiter for &str {
    fn find_next(&self, s: &str) -> Option<(usize, usize)> {
        s.find(self).map(|start| (start, start + self.len()))
    }
}

impl Delimiter for char {
    fn find_next(&self, s: &str) -> Option<(usize, usize)> {
        s.char_indices()
            .find(|(_, c)| c == self)
            .map(|(start, _)| (start, start + self.len_utf8()))
    }
}

pub fn main() {
    for item in StrStrip::new("a b c d", " ") {
        println!("{}", item);
    }
}
