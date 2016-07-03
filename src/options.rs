/* Parse and validate command line arguments. */

use std::env;
use std::io;
use std::process;
use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::{Once, ONCE_INIT};

use getopts::Options;

//use arg_plus;

pub enum ExecutionTarget {
	Stdin,
	File(PathBuf),
	Literal(String)
}

pub struct OptionSpec {
	/* Execution args */
	target: Option<ExecutionTarget>,
	args: Vec<String>,
	repl: bool,
	step_macro: bool,
	trace: bool,
	track_objects: bool,
	trace_set: bool,
	package_path: Option<PathBuf>,
	project_path: Option<PathBuf>,
	dont_need_targets: bool, /* Set indirectly by several options */

	/* Things to do instead of execution */
	disassemble: bool,
	disassemble_verbose: bool,
	print_package: bool,
	print_project: bool,
}


pub static VERSION: &'static str = "0.3b";

lazy_static! {
	pub static ref FULL_VERSION: String = format!("Emily language interpreter: Version {}", VERSION);
	
	pub static ref RUN: OptionSpec = OptionSpec {
		target: None,
		args: vec![],
		repl: false,
		step_macro: false,
		trace: false,
		track_objects: false,
		trace_set: false,
		package_path: None,
		project_path: None,
		
		disassemble: false,
		disassemble_verbose: false,
		print_package: false,
		print_project: false,
	};
}
/*
fn key_mutate_argument(arg: Vec<([&'static str; 2], Spec, String)>) -> Vec<(Vec<arg::Key>, arg::Spec, arg::Doc)> {
	arg_plus::key_mutate(|l| format!("--{}", l.join("-")), arg)
}

fn key_mutate_environment(arg: Vec<([&'static str; 2], Spec, String)>) -> Vec<(Vec<arg::Key>, arg::Spec, arg::Doc)> {
	arg_plus::key_mutate(|l| format!("EMILY_{}", l.iter()
		.map(str::to_uppercase)
		.collect::<Vec<_>>()
		.join("_")
	), arg)
}
*/

static START: Once = ONCE_INIT;

pub fn init() {
	START.call_once(init_);
}

fn init_() -> Result<(), isize> {
	let usage = FULL_VERSION.clone() + "

Sample usage:
	emily filename.em     # Execute program
	emily -               # Execute from stdin
	emily -e \"println 3\"  # Execute from command line
	"
	
	/* Only include this bit if REPL enabled */
	+ if cfg!(BUILD_INCLUDE_REPL) {
	"emily -i              # Run in interactive mode (REPL)
	emily -i filename.em  # ...after executing this program
	"
	}
	else { "" };


	let mut opts = Options::new();

	/* Basic arguments */
	opts.optopt("e", "", "Execute code inline", "<code>");
	if cfg!(BUILD_INCLUDE_REPL) {
		opts.optflagopt("i", "", "Enter interactive mode (REPL)", "<file>");
	}
	opts.optflag("", "machine-version", "Print interpreter version (number only) and quit");
	opts.optflag("v", "version", "Print interpreter version");
	opts.optflag("h", "help", "Display this list of options");

	/* "Config" arguments which can be also set with env vars */
	opts.optflag("", "package-path", "Directory root for packages loaded from \"package\"");
	opts.optflag("", "project-path", "Directory root for packages loaded from \"project\"");

	/* For supporting Emily development itself-- separate out to sort last in help */
	opts.optflag("", "debug-dis", "Print \"disassembled\" code and exit");
	opts.optflag("", "debug-disv", "Print \"disassembled\" code with position data and exit");
	opts.optflag("", "debug-macro", "Print results of each individual macro evaluation");
	opts.optflag("", "debug-trace", "When executing, print interpreter state");
	opts.optflag("", "debug-track", "When executing, give all objects a unique \"!id\" member");
	opts.optflag("", "debug-set", "When executing, print object contents on each set");
	opts.optflag("", "debug-run", "When executing, set all runtime trace type options");
	opts.optflag("", "debug-print-package-path", "Print package loader path and quit");
	opts.optflag("", "debug-print-project-path", "Print project loader path and quit");


	let matches = match opts.parse(env::args().skip(1)) {
		Ok(m)  => m,
		Err(e) => panic!("{}\n{}", e, opts.usage(&usage)),
	};

	if matches.opt_present("help") {
		println!("{}", opts.usage(&usage));
		return;
	}

	if matches.opt_present("version") {
		println!("{}", FULL_VERSION);
		return;
	}

	if matches.opt_present("machine-version") {
		println!("{}", VERSION);
		return;
	}

	unsafe {
		RUN.target = matches.free.get(0)
			.map(|s| match &**s {
				"-" => ExecutionTarget::Stdin,
				path => ExecutionTarget::File(PathBuf::from(path))
			})
			.or(matches.opt_str("e").map(ExecutionTarget::Literal));
	
		RUN.print_version         = matches.opt_present("version");
		RUN.print_machine_version = matches.opt_present("machine-version");
		RUN.disassemble           = matches.opt_present("debug-dis");
		RUN.disassemble_verbose   = matches.opt_present("debug-disv");
		RUN.step_macro            = matches.opt_present("debug-macro");
		RUN.trace                 = matches.opt_present("debug-trace");
		RUN.track_objects         = matches.opt_present("debug-track");
		RUN.trace_set             = matches.opt_present("debug-set");
	
		RUN.package_path = matches.opt_str("package-path")
			.map(OsString::from)
			.or(env::var_os("EMILY_PACKAGE_PATH"))
			.map(PathBuf::from);
	
		RUN.project_path = matches.opt_str("project-path")
			.map(OsString::from)
			.or(env::var_os("EMILY_PROJECT_PATH"))
			.map(PathBuf::from);

		if cfg!(BUILD_INCLUDE_REPL) {
			if matches.opt_present("i") {
				RUN.repl = true;
				RUN.dont_need_targets = true;
			}
		}
	
		if matches.opt_present("debug-run") {
			RUN.trace = true;
			RUN.track_objects = true;
			RUN.trace_set = true;
		}
	
	    if matches.opt_present("debug-print-package-path") {
	    	RUN.print_package = true;
	    	RUN.dont_need_targets = true;
		}
		
	    if matches.opt_present("debug-print-project-path") {
	    	RUN.print_project = true;
	    	RUN.dont_need_targets = true;
		}
	
		RUN.args.extend_from_slice(&matches.free[1..]);
	}

	if !RUN.dont_need_targets && RUN.target.is_none() {
		return Err(1); /* No targets! Fail and print help. */
	}
	//ArgPlus.argParse(args, targetParse, usage, |progArgs|
	
	Ok(())
}
