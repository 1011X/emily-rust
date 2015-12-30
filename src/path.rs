/* Simple path tools */

#[macro_use]
extern crate lazy_static;

use std::io::read_link;
use std::env::current_dir;
use std::path::PathBuf;
use std::path::Path;

/* TODO: This should be normalized. Strongly consider using extunix.realpath instead */

lazy_static! {
	pub static ref BOOT_PATH: PathBuf = read_link(current_dir().unwrap()).unwrap();
}

pub fn executable_relative_path(path: PathBuf) -> PathBuf {
	/* This will work as long as the executable was not executed from $PATH. */
	if path.is_relative() {
		let invoke_path = Path::new(env::args().nth(0).unwrap()).parent().unwrap();
		let exe_path = read_link(if invoke_path.is_relative() {
			let p = current_dir().unwrap();
			p.push(invoke_path);
			p
		} else {
			invoke_path
		});
		exe_path.push(path);
		exe_path
	} else {
		path
	}
}
