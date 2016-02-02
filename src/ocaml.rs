#![feature(fnbox)]

pub struct NotFound;
pub struct InvalidArgument (String);
pub struct Failure (String);

mod arg {
	pub type Key = String;
	pub type Doc = String;
	
	pub enum Spec {
		Unit (Box<FnBox()>),
		String (Box<FnBox(String)>),
	}
	
	pub enum Error {
		Help (String),
		Bad (String),
	}
}

pub fn failwith(s: String) -> Result<(), Failure> {
	Err (Failure (s.clone()))
}
