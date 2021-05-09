use crossbeam;
use crossbeam::channel::bounded;
use rand::Rng;
use std::io::Read;
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

fn why_need_move_in_spane() {
    let v = vec![1, 2, 3];
    let handle = thread::spawn(move || {
        println!("The vector: {:?}", v);
    });

    // if we don't move, the closure holds a referencee to v, and
    // what if this happen?
    // drop(v);

    handle.join().unwrap();
}

fn thread_with_channel() {
    let (tx, rx) = channel();

    let tx1 = tx.clone();

    thread::spawn(move || {
        let vals = vec![
            String::from("hi"),
            String::from("from"),
            String::from("the"),
            String::from("thread"),
        ];

        for val in vals {
            tx1.send(val).unwrap();
            thread::sleep(Duration::from_secs(1));
        }
    });

    thread::spawn(move || {
        let vals = vec![
            String::from("more"),
            String::from("messages"),
            String::from("for"),
            String::from("you"),
        ];

        for val in vals {
            tx.send(val).unwrap();
            thread::sleep(Duration::from_secs(1));
        }
    });

    for received in rx {
        println!("Got: {}", received);
    }
}

fn using_mutex() {
    let m = Mutex::new(5);

    // the MutexGuard num will be droped at the end of the scope.
    // it's the same concetp as scope_lock or lock_guard in c++.
    {
        let mut num = m.lock().unwrap();
        *num = 6;
    }
    println!("m = {:?}", m);
}

// note counter is immutable, but we can mutate the integer in
// Mutex.
// Mutex gives us interior mutability.
// Cell/RefCell are other examples that has interior mutability.

fn sharing_mutex() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}

// note for marker:
// Send means you can transfer the ownership between threads.
// e.g Rc<T> if you clone an Rc<T> and send it over to another thread,
// both threads might modify the counter at the same time, hence race condition.
//
// Sync means for a type T, &T is Send. It's safe to have a value be referenced by
// another thread.
// Types with interior mutablity cannot be sync:w

// find max in two theads
fn find_max(arr: &[i32], thread_num: usize) -> Option<i32> {
    let chunk_size = (arr.len() as f64 / thread_num as f64).round() as usize;

    fn find_max_(arr: &[i32], thread_num: usize, threadshold: usize) -> Option<i32> {
        if arr.len() <= threadshold {
            return arr.iter().cloned().max();
        }

        let bounds = (0..thread_num)
            .map(|n| (n * threadshold, (n + 1) * threadshold))
            .map(|(begin, end)| {
                let end = if end >= arr.len() { arr.len() } else { end };

                (begin, end)
            })
            .collect::<Vec<(usize, usize)>>();

        // can't use closure because the scope there is very important.
        crossbeam::scope(|s| {
            let mut threads = Vec::new();
            let mut results = Vec::new();
            for (begin, end) in bounds {
                let t = s.spawn(move |_| find_max_(&arr[begin..end], thread_num, threadshold));
                threads.push(t);
            }

            for t in threads {
                let v = t.join().unwrap();
                results.push(v);
            }

            results.into_iter().flatten().max()
        })
        .ok()
        .flatten()
    }

    find_max_(arr, thread_num, chunk_size)
}

// fn parallel_pipeline() {
//     // bounded channel that can holds n message at a time.
//     let (snd1, rcv1) = bounded(1);
// }

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test1() {
        let mut rng = rand::thread_rng();
        let vecs = (0..999999999)
            .into_iter()
            .map(|_| rng.gen::<i32>())
            .collect::<Vec<i32>>();

        {
            let start = Instant::now();
            let max = find_max(&vecs[..], 4);
            println!("find max: {:?}, takes: {:.2?}", max, start.elapsed());
        }

        {
            let start = Instant::now();
            let max = vecs.iter().cloned().max();
            println!("real max: {:?}, takes: {:.2?}", max, start.elapsed());
        }
    }
}
