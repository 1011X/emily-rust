pub struct Failure (String);

mod Arg {
	pub type Key = String;
	pub type Doc = String;
	pub enum Spec<'a> {
		Unit (Box<Fn()>),
		Bool (Box<Fn(bool)>),
		Set (&'a mut bool),
		Clear (&'a mut bool),
		String (Box<Fn(String)>),
		SetString (&'a mut String),
		Int (Box<Fn(i32)>),
		SetInt (&'a mut i32),
		Float (Box<Fn(f64)>),
		SetFloat (&'a mut f64),
		Tuple (Vec<Spec>),
		Symbol (
	}
}
