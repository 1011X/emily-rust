/* Simple path tools */
// TODO: optimize away all these allocations

use std::env;
use std::path::PathBuf;

/* TODO: This should be normalized. Strongly consider using extunix.realpath instead */
fn readlink(path: PathBuf) -> PathBuf {
	path.canonicalize().unwrap()
}

lazy_static! {
	pub static ref BOOT_PATH: PathBuf = readlink(env::current_dir().unwrap());
}

pub fn executable_relative_path(path: PathBuf) -> PathBuf {
    /* This will work as long as the executable was not executed from $PATH. */
	if path.is_relative() {
		let invoke_path = env::args().nth(0).map(PathBuf::from).unwrap().parent().unwrap().to_path_buf();
		let mut exe_path = readlink(if invoke_path.is_relative() {
			let mut cwd = env::current_dir().unwrap();
			cwd.push(invoke_path);
			cwd
		} else {
			invoke_path.to_path_buf()
		});
		exe_path.push(path);
		exe_path
	} else {
		path
	}
}
