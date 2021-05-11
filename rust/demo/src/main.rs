pub mod borrow;
pub mod macros;
pub mod other;
pub mod stdlib;

#![feature(trace_macros)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitfield;

#[macro_use]
extern crate bitflags;

fn main() {
    println!("Hello, world!");
}
