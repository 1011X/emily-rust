/* Simple path tools */

/* TODO: This should be normalized. Strongly consider using extunix.realpath instead */

fn readlink(path) {
	FileUtil.readlink(path)
}

lazy_static! {
	static ref BOOT_PATH :  = readlink(Sys.getcwd());
}

fn executableRelativePath(path) {
	/* This will work as long as the executable was not executed from $PATH. */
	if FilePath.is_relative(path) {
		let invokePath = Filename.dirname(Array.get(Sys.argv, 0));
		let exePath = readlink(if FilePath.is_relative(invokePath) {
			Filename.concat (Sys.getcwd()) invokePath } else { invokePath });
		in Filename.concat exePath path
	}
	else { path }
}
