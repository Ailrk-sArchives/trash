use crossbeam;
use crossbeam::channel::bounded;
use rand::Rng;
use std::io::Read;
use std::sync::mpsc::channel;
use std::thread;
use std::time::{Duration, Instant};

fn thread_with_channel() {
    // multiproducer single consumer communication primitive.
    let (tx, rx) = channel();
    // sender can be cloned, but only one reciver is allowed.
    let tx1 = tx.clone();

    let sender1 = thread::spawn(move || {
        tx.send("Hello thread".to_owned())
            .expect("Unabel to send on channel");
    });

    let sender2 = thread::spawn(move || {
        tx1.send("Hello thread".to_owned())
            .expect("Unabel to send on channel");
    });

    let receiver = thread::spawn(move || {
        let value = rx.recv().expect("Unable to receive from channel");
        print!("{}", value);
    });

    sender1.join().expect("Sender1 has painicked");
    sender2.join().expect("Sender2 has painicked");
    receiver.join().expect("The receivere thread has painicked");
}

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
                println!("thread!!");
            }

            for t in threads {
                let v = t.join().unwrap();
                results.push(v);
            }

            results.into_iter().flatten().max()
        })
        .ok()
        .and_then(|e| e)
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
        let vecs = (0..99999999)
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
