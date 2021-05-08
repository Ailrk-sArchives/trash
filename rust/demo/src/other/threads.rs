use std::io::Read;
use std::sync::mpsc::channel;
use std::thread;

fn thread_with_channel() {
    let (tx, rx) = channel();

    let sender = thread::spawn(move || {
        tx.send("Hello thread".to_owned())
            .expect("Unabel to send on channel");
    });

    let receiver = thread::spawn(move || {
        let value = rx.recv().expect("Unable to receive from channel");
        print!("{}", value);
    });

    sender.join().expect("The sender has painicked");
    receiver.join().expect("The receivere thread has painicked");
}

// find max in two theads
fn find_max(arr: &[i32], thread_num: usize, threadshold: usize) -> Option<i32> {
    if arr.len() <= threadshold as usize {
        return arr.iter().cloned().max();
    }

    let chunk_size = (arr.len() as f64 / thread_num as f64).round() as usize;

    let (tx, rx) = channel();

    (0..thread_num)
        .map(|n| n + 1)
        .map(|idx| {
            let chop = idx * chunk_size;
            if chop > arr.len() {
                arr.len()
            } else {
                chop
            }
        })
        .map(|idx| thread::spawn(move || {}))
        .map(|t| {
            t.join().unwrap();
        });

    todo!()
}
