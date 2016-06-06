/* Parse and validate command line arguments. */

use std::env;
use std::io;
use std::path;
use std::process;
use std::sync::{Once, ONCE_INIT};

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
	print_version: bool,
	print_machine_version: bool
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
		print_version: false,
		print_machine_version: false
	};
}

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

fn build_path_set_spec<F: Fn(String)>(name: [&'static str; 2], action: F, what_is: &'static str) -> ([&'static str; 2], Spec, String) {
	(name, Spec::String(action), format!("Directory root for packages loaded from \"{}\"", what_is))
}


static START: Once = ONCE_INIT;

pub fn init() {
START.call_once(|| {
	let usage = FULL_VERSION.clone() + "

Sample usage:
    emily filename.em     # Execute program
    emily -               # Execute from stdin
    emily -e \"println 3\"  # Execute from command line"

/* Only include this bit if REPL enabled */
+ if cfg!(BUILD_INCLUDE_REPL) { "
    emily -i              # Run in interactive mode (REPL)
    emily -i filename.em  # ...after executing this program"
} else { "" };
	
	let mut execute_args = vec![ /* Basic arguments */
		("-", Arg.Unit(|| { /* Arg's parser means the magic - argument must be passed in this way. */
			RUN.target = Some(ExecutionTarget::Stdin);
			Err(arg_plus::Complete)
		}), ""), /* No summary, this shouldn't be listed with options. */

		/* Args */
		("-e", Arg.String(|f| {
			RUN.target = Some(ExecutionTarget::Literal(f));
			Err(ArgPlus.Complete)
		}), "Execute code inline")
	];

	let environment_args = [ /* "Config" arguments which can be also set with env vars */
		build_path_set_spec(["package", "path"], |a| RUN.package_path = Some(a), "package"),
		build_path_set_spec(["project", "path"], |a| RUN.project_path = Some(a), "project")
	];

	let args = {
		let mut c = execute_args.clone();
		c.push_all(*key_mutate_argument(environment_args));
		c.push_all(&debug_args);
		c
	};

	let targetParse = |t| {
		RUN.target = ExecutionTarget::File(t);
		Err(ArgPlus.Complete)
	};
	
	
	ArgPlus.envParse(key_mutate_environment(environment_args));
	
	
	
	
	
	let mut opts = Options::new();
	
	// default args
	opts.optflag("v", "version", "Print interpreter version");
	opts.optflag("h", "help", "Display this list of options");
	
	// execute args
	opts.optopt("e", "", "Execute code inline", "<code>");
	if cfg!(BUILD_INCLUDE_REPL) {
		opts.optflagopt("i", "", "Enter interactive mode (REPL)", "<file>");
	}
	opts.optflag("", "machine-version", "Print interpreter version (number only) and quit");
	
	// environment args
	//opts.optflag("", "package-path", "Directory root for packages loaded from \"package\"");
	//opts.optflag("", "project-path", "Directory root for packages loaded from \"project\"");
	
	// debug args
	opts.optflag("", "debug-dis", "Print \"disassembled\" code and exit");
	opts.optflag("", "debug-disv", "Print \"disassembled\" code with position data and exit");
	opts.optflag("", "debug-macro", "Print results of each individual macro evaluation");
	opts.optflag("", "debug-trace", "When executing, print interpreter state");
	opts.optflag("", "debug-track", "When executing, give all objects a unique \"!id\" member");
	opts.optflag("", "debug-set", "When executing, print object contents on each set");
	opts.optflag("", "debug-run", "When executing, set all runtime trace type options");
	opts.optflag("", "debug-print-package-path", "Print package loader path and quit");
    opts.optflag("", "debug-print-project-path", "Print project loader path and quit");
	
	
	let arguments = env::args().skip(1);
	
	let matches = match opts.parse(arguments) {
		Ok(m) => m,
		Err(f) => panic!("{}\n{}", f, opts.usage(&usage)),
	};
	
	unsafe {
		if matches.free.contains(&"-".to_string()) {
			RUN.target = Some(ExecutionTarget::Stdin);
			//Err(arg_plus::Complete)
		}
		
		if let Some(arg) = matches.opt_str("e") {
			RUN.target = Some(ExecutionTarget::Literal(arg));
			//Err(arg_plus::Complete)
		} else {
			println!("emily: option '-e' needs an argument.");
			println!("{}", opts.usage(&usage));
		}
	
		if cfg!(BUILD_INCLUDE_REPL) {
			if matches.opt_present("i") {
				RUN.repl = true;
				RUN.dont_need_targets = true;
			}
		}
		
		if matches.opt_present("machine-version") {
			RUN.print_machine_version = true;
		}
		
		if matches.opt_present("debug-dis") {
			RUN.disassemble = true;
		}
		
		if matches.opt_present("debug-disv") {
			RUN.disassemble_verbose = true;
		}
		
		if matches.opt_present("debug-macro") {
			RUN.step_macro = true;
		}
		
		if matches.opt_present("debug-trace") {
			RUN.trace = true;
		}
		
		if matches.opt_present("debug-track") {
			RUN.track_objects = true;
		}
		
		if matches.opt_present("debug-set") {
			RUN.trace_set = true;
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
	}
	
	
	
	
	
	ArgPlus.argParse(args, targetParse, usage, |progArgs|
		/* Arguments are parsed; either short-circuit with an informational message, or store targets */
		if RUN.print_machine_version {
			println!("{}", VERSION);
		}
		else if RUN.print_version {
			println!("{}", FULL_VERSION);
		}
		else {
			RUN.args = progArgs;
			if !RUN.dont_need_targets {
				if let None = RUN.target {
					Err (arg_plus::Error::Help (1)) /* No targets! Fail and print help. */
				}
			}
		}
	);
});

}
