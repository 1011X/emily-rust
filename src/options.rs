/* Parse and validate command line arguments. */

#[macro_use]
extern crate lazy_static;
extern crate getopts;

use std::env;
use std::io;
use std::process;
use std::sync;

static VERSION: &'static str = "0.3b";

lazy_static! {
	static ref FULL_VERSION : String = format!("Emily language interpreter: Version {}", VERSION);
}

pub enum ExecutionTarget {
	Stdin,
	File (String),
	Literal (String)
}

pub struct OptionSpec {
	/* Execution args */
	target : Option<ExecutionTarget>,
	args : Vec<String>,
	repl : bool,
	step_macro : bool,
	trace : bool,
	track_objects : bool,
	trace_set : bool,
	package_path : Option<String>,
	project_path : Option<String>,
	dont_need_targets : bool, /* Set indirectly by several options */

	/* Things to do instead of execution */
	disassemble : bool,
	disassemble_verbose : bool,
	print_package : bool,
	print_project : bool,
	print_version : bool,
	print_machine_version : bool
}

lazy_static! {
	pub static ref RUN : OptionSpec = OptionSpec {
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

fn key_mutate_argument() -> {
	arg_plus::key_mutate(|l| format!("--{}", l.join("-")))
}

fn key_mutate_environment() -> {
	arg_plus::key_mutate(|l|
		format!("EMILY_{}", l.iter()
			.map(|s| s.to_uppercase())
			.collect::<Vec<_>>()
			.join("_")
		)
	)
}

fn build_path_set_spec<F: Fn(String)>(name: [&'static str; 2], action: F, whatIs: &'static str) -> ([&'static str; 2], Spec, String) {
	(name, Spec::String(action), format!("Directory root for packages loaded from \"{}\"", whatIs))
}


static START: sync::Once = sync::ONCE_INIT;

START.call_once(|| {
	let usage = *FULL_VERSION + "

Sample usage:
    emily filename.em     # Execute program
    emily -               # Execute from stdin
    emily -e \"println 3\"  # Execute from command line"

/* Only include this bit if REPL enabled */
+ if cfg!(BUILD_INCLUDE_REPL) { "
    emily -i              # Run in interactive mode (REPL)
    emily -i filename.em  # ...after executing this program"
else { "" } + "

Options:";
	/*
	let mut execute_args = vec![ /* Basic arguments */
		("-", Arg.Unit(|| { /* Arg's parser means the magic - argument must be passed in this way. */
			RUN.target = Some(ExecutionTarget::Stdin);
			Err(arg_plus::Complete)
		}), ""), /* No summary, this shouldn't be listed with options. */

		/* Args */
		("-e", Arg.String(|f| {
			RUN.target = Some(ExecutionTarget::Literal (f));
			Err(ArgPlus.Complete)
		}), "Execute code inline")
	];
	
	if cfg!(BUILD_INCLUDE_REPL) {
		execute_args.push(
			("-i", Arg.Unit(|f| {
				RUN.repl = true;
				RUN.dont_need_targets = true;
			}), "Enter interactive mode (REPL)")
		);
	}
	
	execute_args.push_all(&[

		("--machine-version", Arg.Unit(|| {
			RUN.print_machine_version = true;
		}), "Print interpreter version (number only) and quit")
	]);

	let environment_args = [ /* "Config" arguments which can be also set with env vars */
		build_path_set_spec(["package", "path"], |a| RUN.package_path = Some (a), "package"),
		build_path_set_spec(["project", "path"], |a| RUN.project_path = Some (a), "project")
	];
	
	let debug_args = [ /* For supporting Emily development itself-- separate out to sort last in help */
		("--debug-dis", Arg.Unit(|| RUN.disassemble = true), "Print \"disassembled\" code and exit"),
		("--debug-disv", Arg.Unit(|| RUN.disassemble_verbose = true), "Print \"disassembled\" code with position data and exit"),
		("--debug-macro", Arg.Unit(|| RUN.step_macro = true), "Print results of each individual macro evaluation"),
		("--debug-trace", Arg.Unit(|| RUN.trace = true), "When executing, print interpreter state"),
		("--debug-track", Arg.Unit(|| RUN.track_objects = true), "When executing, give all objects a unique \"!id\" member"),
		("--debug-set", Arg.Unit(|| RUN.trace_set = true), "When executing, print object contents on each set"),
		("--debug-run", Arg.Unit(|| {
			RUN.trace = true;
			RUN.track_objects = true;
			RUN.trace_set = true;
		}),  "When executing, set all runtime trace type options")
	];

	let args = {
		let mut c = execute_args.clone();
		c.push_all(*key_mutate_argument(environment_args));
		c.push_all(&debug_args);
		c
	};

	let targetParse = |t| {
		RUN.target = ExecutionTarget::File (t);
		Err(ArgPlus.Complete)
	};
	*/
	
	ArgPlus.envParse(key_mutate_environment(environment_args));
	
	
	
	
	/*
	let mut opts = Options::new();
	opts.optopt("e", "", "Execute code inline", "<code>");
	
	if cfg!(BUILD_INCLUDE_REPL) {
		opts.optflagopt("i", "", "Enter interactive mode (REPL)", "<file>");
	}
	
	opts.optflag("v", "version", "Print interpreter version");
	opts.optflag("h", "help", "Display this list of options");
	opts.optflag("", "machine-version", "Print interpreter version (number only) and quit");
	//opts.optflag("", "package-path", "Directory root for packages loaded from \"package\"");
	//opts.optflag("", "project-path", "Directory root for packages loaded from \"project\"");
	opts.optflag("", "debug-dis", "Print \"disassembled\" code and exit");
	opts.optflag("", "debug-disv", "Print \"disassembled\" code with position data and exit");
	opts.optflag("", "debug-macro", "Print results of each individual macro evaluation");
	opts.optflag("", "debug-trace", "When executing, print interpreter state");
	opts.optflag("", "debug-track", "When executing, give all objects a unique \"!id\" member");
	opts.optflag("", "debug-set", "When executing, print object contents on each set");
	opts.optflag("", "debug-run", "When executing, set all runtime trace type options");
	//opts.optflag("", "debug-print-package-path", "Print package loader path and quit");
    //opts.optflag("", "debug-print-project-path", "Print project loader path and quit");
	
	
	let arguments: Vec<String> = env::args().collect();
	
	let matches = match opts.parse(&arguments[1..]) {
		Ok (m) => m,
		Err (f) => {
			writeln!(io::stderr(), "{}", f);
			writeln!(io::stderr(), "{}", opts.usage(&opts.short_usage(&arguments[0])));
			process::exit(1);
		}
	};
	
	if let Some(arg) = matches.opt_str("e") {
		RUN.target = Some (ExecutionTarget::Literal (arg));
	}
	
	if cfg!(BUILD_INCLUDE_REPL) {
		
	}
	
	RUN.
	
	if matches.free.contains(&"-".to_string()) {
		RUN.target = Some(ExecutionTarget::Stdin);
	}
	*/
	
	
	
	
	
	
	ArgPlus.argParse(args, targetParse, usage, |progArgs|
		/* Arguments are parsed; either short-circuit with an informational message, or store targets */
		if RUN.print_machine_version {
			println!(VERSION);
		}
		else if RUN.print_version {
			println!(FULL_VERSION);
		}
		else {
			RUN.args = progArgs;
			if !RUN.dont_need_targets {
				if let None = RUN.target {
					Err (ArgPlus.Help 1) /* No targets! Fail and print help. */
				}
			}
		}
	);
});
