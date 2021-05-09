// dynamic dispatch with trait object
// how do we achieve subtypeing?

pub trait Draw {
    fn draw(&self);
}

// use trait oject.
// same as hide different types in a constructor with exitential type
// in haskell.
pub struct Screen {
    pub components: Vec<Box<dyn Draw>>,
}

impl Screen {
    pub fn run(&self) {
        for components in self.components.iter() {
            components.draw();
        }
    }
}

// use trait bound
pub struct Screen1<T: Draw> {
    pub components: Vec<T>,
}

impl<T> Screen1<T>
where
    T: Draw,
{
    pub fn run(&self) {
        for components in self.components.iter() {
            components.draw();
        }
    }
}
