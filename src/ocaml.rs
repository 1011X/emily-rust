use std::boxed::FnBox;

pub struct NotFound;
pub struct EndOfFile;
pub struct InvalidArgument(String);
pub struct Failure(String);

pub mod arg {
	pub type Key = String;
	pub type Doc = String;
	
	pub enum Spec {
		Unit(Box<FnBox()>),
		String(Box<FnBox(String)>),
	}
	
	pub enum Error {
		Help(String),
		Bad(String),
	}
}

pub mod sys {
	pub struct Break;
}

pub fn failwith(s: &str) -> Result<(), Failure> {
	Err(Failure(s.to_owned()))
}
