#![feature(slice_patterns)]
#![allow(dead_code)]
#![allow(unused_variables)]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
#[cfg(feature = "repl")]
extern crate ctrlc;

mod value;
//mod value_util;
mod pretty;
mod token;
//mod tokenize;
mod options;
mod path;
#[cfg(feature = "repl")]
mod repl;

fn main() {
	options::init();
}
