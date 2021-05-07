use std::thread;
use std::io::Read;

fn download(url: &str) {

}

fn get_two_sites() {
    let thread1 = thread::spawn(|| download("https://www.google.com"));
}
