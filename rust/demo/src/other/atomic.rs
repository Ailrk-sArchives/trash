use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;

macro_rules! wait {
    ( $lock:expr ) => {
        while $lock.load(Ordering::SeqCst) != 0 {}
    };
}

macro_rules! lock {
    () => {
        Arc::new(AtomicUsize::new(1))
    };
}

macro_rules! clone_lock {
    ( $lock:expr ) => {
        Arc::clone(&$lock)
    };
}

fn atomic_demo1() {
    let spinlock = lock!();
    let spinlock_clone = clone_lock!(&spinlock);

    let thread = thread::spawn(move || {
        spinlock_clone.store(0, Ordering::SeqCst);
    });

    wait!(spinlock);

    if let Err(panic) = thread.join() {
        println!("Thread has an error {:?}", panic);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        atomic_demo1();
    }
}
