/* Parse and validate command line arguments. */

use getopts::Options;

use std::env;
//use std::io;
use std::process::exit;
use std::ffi::OsString;
use std::path::PathBuf;
use std::sync::{RwLock, Once, ONCE_INIT};

use lazy_static::lazy_static;

//use arg_plus;

#[derive(Clone)]
pub enum ExecutionTarget {
    Stdin,
    File(PathBuf),
    Literal(String),
}

#[derive(Default)]
pub struct OptionSpec {
    /* Execution args */
    pub target: Option<ExecutionTarget>,
    pub args: Vec<String>,
    pub repl: bool,
    pub step_macro: bool,
    pub trace: bool,
    pub track_objects: bool,
    pub trace_set: bool,
    pub package_path: Option<PathBuf>,
    pub project_path: Option<PathBuf>,

    /* Things to do instead of execution */
    pub disassemble: bool,
    pub disassemble_verbose: bool,
    pub print_package: bool,
    pub print_project: bool,
    pub print_version: bool,
    pub print_machine_version: bool,
}


pub static VERSION: &str = "0.3b";

lazy_static! {
    pub static ref FULL_VERSION: String = format!("Emily language interpreter: Version {}", VERSION);
    pub static ref RUN: RwLock<OptionSpec> = RwLock::new(OptionSpec::default());
}

static START: Once = ONCE_INIT;

pub fn init() {
    START.call_once(init_);
}

// TODO This doesn't currently behave the way the original code does. All
// targets are evaluated first, *then* the version is printed or the 
fn init_() {
    let usage = FULL_VERSION.clone() + "

Sample usage:
    emily filename.em     # Execute program
    emily -               # Execute from stdin
    emily -e \"println 3\"  # Execute from command line"
    
    /* Only include this bit if REPL enabled */
    + if cfg!(feature = "repl") {
    "
    emily -i              # Run in interactive mode (REPL)
    emily -i filename.em  # ...after executing this program"
    }
    else { "" };


    let mut opts = Options::new();

    /* Basic arguments */
    opts.optopt("e", "", "Execute code inline", "<code>");
    if cfg!(feature = "repl") {
        opts.optflag("i", "", "Enter interactive mode (REPL)");
    }
    opts.optflag("", "machine-version", "Print interpreter version (number only) and quit");
    opts.optflag("v", "version", "Print interpreter version");
    opts.optflag("h", "help", "Display this list of options");

    /* "Config" arguments which can be also set with env vars */
    opts.optopt("", "package-path", "Directory root for packages loaded from \"package\"", "<path>");
    opts.optopt("", "project-path", "Directory root for packages loaded from \"project\"", "<path>");

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


    let mut matches = match opts.parse(env::args().skip(1)) {
        Ok(m)  => m,
        Err(e) => {
            eprintln!("{}", e);
            eprintln!("{}", opts.usage(&usage));
            exit(1);
        }
    };

    if matches.opt_present("help") {
        println!("{}", opts.usage(&usage));
        exit(0);
    }
    
    if matches.opt_present("machine-version") {
        println!("{}", VERSION);
        exit(0);
    }
    
    if matches.opt_present("version") {
        println!("{}", *FULL_VERSION);
        exit(0);
    }
    
    let mut run = RUN.write().unwrap();
    
    run.print_version         = matches.opt_present("version");
    run.print_machine_version = matches.opt_present("machine-version");
    run.disassemble           = matches.opt_present("debug-dis");
    run.disassemble_verbose   = matches.opt_present("debug-disv");
    run.step_macro            = matches.opt_present("debug-macro");
    run.trace                 = matches.opt_present("debug-trace");
    run.track_objects         = matches.opt_present("debug-track");
    run.trace_set             = matches.opt_present("debug-set");

    run.package_path = matches.opt_str("package-path")
        .map(OsString::from)
        .or(env::var_os("EMILY_PACKAGE_PATH"))
        .map(PathBuf::from);

    run.project_path = matches.opt_str("project-path")
        .map(OsString::from)
        .or(env::var_os("EMILY_PROJECT_PATH"))
        .map(PathBuf::from);
    
    run.repl = cfg!(feature = "repl") && matches.opt_present("i");

    if matches.opt_present("debug-run") {
        run.trace = true;
        run.track_objects = true;
        run.trace_set = true;
    }

    run.print_package = matches.opt_present("debug-print-package-path");
    run.print_project = matches.opt_present("debug-print-project-path");
    
    
    // original code chooses target on whether -e or a file comes first; we
    // can't do that with getopts easily, so prioritize -e over files. also,
    // Vec::remove panics if out-of-range and there's no equivalent that uses an
    // option (yet), so checking if vector isn't empty is necessary.
    if !matches.free.is_empty() {
        run.target = Some(match &matches.free.remove(0)[..] {
            "-" => ExecutionTarget::Stdin,
            path => ExecutionTarget::File(PathBuf::from(path))
        });
    }
    
    run.target = matches.opt_str("e").map(ExecutionTarget::Literal);
    run.args = matches.free;
    
    let dont_need_targets = run.print_project || run.print_package || run.repl;
    if !dont_need_targets && run.target.is_none() {
        /* No targets! Fail and print help. */
        eprintln!("{}", opts.usage(&usage));
        exit(1);
    }
}
