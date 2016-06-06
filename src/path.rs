/* Simple path tools */

use std::fs;
use std::env;
use std::path::{Path, PathBuf};

/* TODO: This should be normalized. Strongly consider using extunix.realpath instead */

lazy_static! {
	pub static ref BOOT_PATH: PathBuf = env::current_dir().unwrap();
}

pub fn executable_relative_path(val: &'static str) -> PathBuf {
	/* This will work as long as the executable was not executed from $PATH. */
	let path = PathBuf::from(val);
	
	if path.is_relative() {
		let invoke_path = Path::new(env::args().nth(0).unwrap()).parent().unwrap();
		let exe_path = fs::canonicalize(if invoke_path.is_relative() {
			let p = env::current_dir().unwrap();
			p.push(invoke_path);
			p 	
		} else {
			invoke_path
		}).unwrap();
		exe_path.push(path);
		exe_path
	} else {
		path
	}
}
