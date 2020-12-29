use std::mem;

pub struct List {
    head: Link,
}

/* 1. Recursive definition must be boxed.
 *    Because we don't know how much memory to allocate for the
 *    recursive type.
 *    If the recursive part is boxed, the memory for the box is
 *    definite.
 *    (Error: recursive type has infinite size)
 *
 * 2. Be aware of when things are on the heap and when are on the stack.
 *    Here the first element is a link, which either be empty or be a ptr
 *    to a node.
 *    Reason we write like this is because we want all data allocated on
 *    the heap, but only the handler on the stack.
 *    (This will not be a problem in GC'd languages).
 */
enum Link {
    Empty,
    More(Box<Node>),
}

struct Node {
    elem: i32,
    next: Link,
}

impl List {
    pub fn new() -> Self {
        List { head: Link::Empty }
    }

    // 3. You can just write next: self.head here.
    //    It violate the borrow checker rule.
    //    Why? assume it's allowed, then when the function exit
    //    push need to give the borrow back to the owner, self
    //    will be left partially initialized.
    //    Solution is to use replace, which guarantee the borrow
    //    is still valid but still allow us to get the value of
    //    the borrow.
    pub fn push(&mut self, elem: i32) {
        let new_node = Box::new(Node {
            elem,
            next: mem::replace(&mut self.head, Link::Empty),
        });

        self.head = Link::More(new_node);
    }

    pub fn pop(&mut self) -> Option<i32> {
        match mem::replace(&mut self.head, Link::Empty) {
            Link::Empty => None,
            Link::More(node) => {
                self.head = node.next;
                Some(node.elem)
            }
        }
    }
}

impl Drop for List {
    fn drop(&mut self) {
        let mut cur_liink = mem::replace(&mut self.head, Link::Empty);

        while let Link::More(mut boxed_node) = cur_liink {
            cur_liink = mem::replace(&mut boxed_node.next, Link::Empty);
        }
    }
}

#[cfg(test)]
mod test {
    use super::List;

    #[test]
    fn basics() {
        let mut list = List::new();
        assert_eq!(list.pop(), None);
        // Populate list
        list.push(1);
        list.push(2);
        list.push(3);

        // Check normal removal
        assert_eq!(list.pop(), Some(3));
        assert_eq!(list.pop(), Some(2));

        // Push some more just to make sure noting's corrupted
        list.push(4);
        list.push(5);

        // Check normal removal
        assert_eq!(list.pop(), Some(5));
        assert_eq!(list.pop(), Some(4));

        // Check exhaustion
        assert_eq!(list.pop(), Some(1));
        assert_eq!(list.pop(), None);
    }
}
