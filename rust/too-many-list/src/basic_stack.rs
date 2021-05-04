use std::mem;

#[derive(Debug)]
pub enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}

macro_rules! assume_init {
    () => {{
        mem::MaybeUninit::uninit().assume_init()
    }};
}

impl<T> List<T> {
    pub fn new() -> Self {
        List::Nil
    }

    // what's the take away?
    // if you want to move out a value from a mutable reference, technically
    // you can't. but actually you can! With replace, you can sneak in another
    // value to avoid dangling ptr, and get the ownership of the content of the
    // mutable reference.
    pub fn push(&mut self, elem: T) {
        match mem::replace(self, Self::Nil) {
            Self::Nil => (),
            cons => {
                let new_list = Self::Cons(elem, Box::new(cons));
                *self = new_list;
            }
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        match mem::replace(self, Self::Nil) {
            Self::Nil => None,
            Self::Cons(ref mut x, ref mut xs) => {
                let elem = mem::replace(x, unsafe { assume_init!() });
                *self = *mem::replace(xs, Box::new(Self::Nil));
                Some(elem)
            }
        }
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        match self {
            List::Nil => (),
            List::Cons(_, xs) => {
                drop(xs.as_mut());
                drop(xs);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn basics() {
        let list: List<i32> = List::Cons(1, Box::new(List::Cons(2, Box::new(List::Nil))));
        println!("{:?}", list);
    }

    #[test]
    fn test1() {
        let mut list: List<i32> = List::Cons(1, Box::new(List::Cons(2, Box::new(List::Nil))));
        list.push(1);
        list.push(2);
        list.push(3);

        assert_eq!(list.pop().unwrap(), 3);
        assert_eq!(list.pop().unwrap(), 2);
        assert_eq!(list.pop().unwrap(), 1);
        list.push(1);
        list.push(2);
        list.push(3);
        list.push(4);
        list.push(5);

        println!("{:?}", list);
    }
}
