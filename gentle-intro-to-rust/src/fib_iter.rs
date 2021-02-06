pub struct Fibnacci {
    curr: u64,
    next: u64
}

impl Fibnacci {
    pub fn new() -> Fibnacci {
        Fibnacci { curr: 0, next: 1}
    }
}

impl Iterator for Fibnacci {
    type Item = u64;
    fn next(&mut self) -> Option<u64> {
        let new_next = self.curr + self.next;
        self.curr = self.next;
        self.next = new_next;
        Some(self.curr)
    }
}
