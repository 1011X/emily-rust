/* Loads a program and runs it, based on contents of Options. */

mod arg_plus;
mod execute;
mod macros;
mod value;

mod options;
mod tokenize;
mod token;
mod loader;
mod pretty;
mod repl;

use std::fs;
use std::io;

use options::ExecutionTarget;
use token::CodeSource;
use loader::LoadLocation;

fn main() {
    let process_one = |target| {
        let buf = match target {
            ExecutionTarget::File (f) =>
            	// TODO: handle .unwrap() assertion below
            	tokenize::tokenize_channel(CodeSource::File (f), fs::File::open(f).unwrap()),
            ExecutionTarget::Stdin =>
            	tokenize::tokenize_channel(CodeSource::Stdin, io::stdin()),
            ExecutionTarget::Literal (s) =>
            	tokenize::tokenize_string(CodeSource::Cmdline, s.clone()),
        };
        
        let location = match target {
            ExecutionTarget::File (f) => loader::location_around(f),
            _ => LoadLocation::Cwd,
        };
        /*  */
		if options::RUN.disassemble {
			println!("{}", pretty::dump_code_tree_terse(buf));
		}
		else if options::RUN.disassemble_verbose {
			println!("{}", pretty::dump_code_tree_dense(buf));
		}
		else if options::RUN.print_package {
			println!("{}", loader::package_root_path());
		}
		else if options::RUN.print_project {
			println!("{}", loader::project_path_for_location(location).display());
		}
		else {
			loader::execute_program_from(location, buf);
		}
    };
    
	if options::RUN.repl && cfg!(BUILD_INCLUDE_REPL) {
		repl::repl(&options::RUN.target);
	}
	else {
	    /* FIXME: This is maybe awkward? It is here so print_package can work without a target. */
	    /* It works by assuming an implicit -e '', which is only safe if we assume */
	    /* option.ml would have failed already if that weren't ok. */
		let result = process_one(options::RUN.target
			.as_ref()
			.unwrap_or(ExecutionTarget::Literal ("".to_string()))
		);
		
		match result {
			Err (EmilyError::CompilationError (e)) |
			Err (EmilyError::Failure (e)) => {
				writeln!(io::stderr(), "{}", e);
				exit(1);
			}
			_ => {}
		}
		
        /* In the standalone version, it appears this happens automatically on exit. */
        /* In the C-embed version, it does *not*, so call it here. */
        flush_all();
	}
}
