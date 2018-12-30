#![feature(slice_patterns)]
#![feature(box_syntax)]

#![allow(dead_code)]
#![allow(unused_variables)]

//mod macros;
mod options;
mod path;
mod pretty;
mod token;
#[cfg(feature = "repl")]
mod repl;
//mod tokenize;
mod value;
mod value_util;

fn main() {
    options::init();
}
