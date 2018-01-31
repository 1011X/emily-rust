#![feature(slice_patterns)]

#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;

mod value;
mod pretty;
mod token;

fn main() {
	println!("Hello!");
}
