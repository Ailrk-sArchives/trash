/// Rust practice.

#[derive(Debug)]
pub struct StrStrip<'a, T> {
    rem: Option<&'a str>,
    del: T,
}

impl<'a, T> StrStrip<'a, T> {
    pub fn new(haystack: &'a str, del: T) -> Self {
        Self {
            rem: Some(haystack),
            del,
        }
    }
}

pub trait Delimiter {
    fn find_next(&self, s: &str) -> Option<(usize, usize)>;
}

impl<'a, T: Delimiter> Iterator for StrStrip<'a, T> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

impl Delimiter for &str {
    fn find_next(&self, s: &str) -> Option<(usize, usize)> {
        unimplemented!()
    }
}

impl Delimiter for char {
    fn find_next(&self, s: &str) -> Option<(usize, usize)> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
