#![feature(slice_patterns)]
#![allow(dead_code, unused_variables)]

#[macro_use] extern crate nom;
#[macro_use] extern crate lazy_static;

mod value;
mod pretty;
mod token;

fn main() {
	println!("Hello!");
}
