/* This file contains an routine that runs an emily repl off stdin. Once entered, it runs until quit. */
/* In future, this could possibly be moved into its own standalone executable. */

extern crate ctrlc;
use ctrlc::CtrlC;

use std::io;

use options::ExecutionTarget;

/* Predefined Emily code used by the REPL */
lazy_static! {
	pub static ref REPL_HELP_STRING: String = options::FULL_VERSION + ", interactive mode\
		Type \"help\" for help, \"quit\" to quit\
		Type \"last\" to get the previous line's value";
}

/* Check if the string 's' ends with a backslash. */
/* FIXME: This is inadequate, the tokenizer should report this itself. */
pub fn is_continued(s: &str) -> bool {
    s.ends_with('\\')
}

pub static LAST_KEY_STRING: &'static str = "last";


/* Runs the REPL.

This is the high-level entry point that Main uses to run the REPL.
It needs to set up a global scope to use, then execute the
files provided as arguments (if any) and then start reading
input from the user.

Control-D (EOF) exits the REPL, as does evaluating the word `quit`. */

pub fn repl(target: &Option<ExecutionTarget>) {

    /* This is our global mutable REPL scope. Prepopulate it with some stuff: */
    let starter = loader::complete_starter(LoadLocation::Cwd);
    let mut scope_table = value::table_from(starter.root_scope);
    let mut stdout = io::stdout();
    let mut stdin = io::stdin();

    /* Line and lines are used to read and execute user input */
    let mut line = "".to_string();
    let mut lines = vec![];

    /* Display a prompt, then read a line from the user */
    // &str -> io::Result<()>
    let prompt_and_read_line = |s| {
        print!("{}", s);
        
        let result = stdout.flush()
        	.and(stdin.read_line(line));
    	
        if result.is_ok() {
        	lines.push(line);
    	}
    	
    	result
    };

    /* Tokenize and execute the given file target */
    let run_file = |f| {
        let buf = tokenize::tokenize_channel(CodeSource::File (f), fs::File::open(f).unwrap());
        execute::execute(starter, buf);
    };

    let run_string = |s| {
        let buf = tokenize::tokenize_string(CodeSource::Cmdline (s));
        execute::execute(starter, buf);
    };

    /* Run file or -e target */
    // ExecutionTarget -> Result<(), &'static str>
    let run_target_file = |t| match t {
        ExecutionTarget::File (f) => Ok (run_file(f)),
        ExecutionTarget::Literal (s) => Ok (run_string(s)),
        ExecutionTarget::Stdin => Err ("Can't take input from stdin and run in interactive mode at once."),
    };

    /* Load any files provided by the user, before launching REPL */
    // () -> Result<(), String>
    let run_user_files = || if let &Some (t) = target { run_target_file(t) } else { Ok (()) };

	// () -> Result<(), tokenize::Error>
    let prompt_and_read_buffer = || loop {
        /* Print a prompt, take a line of text, push to "lines" stack */
        prompt_and_read_line(match &*lines { [] => ">>> ", _ => "..> " });

        /* Turn the line stack into a "program" */
        let combined_string = lines.iter()
        	.cloned()
        	.rev()
        	.collect::<Vec<_>>()
        	.join("\n");

        /* Attempt to parse the program and return an AST. */
        match tokenize::tokenize_string(CodeSource::Cmdline, combined_string) {
        	/* The program is invalid, but could be valid with more text. Read another line. */
        	Err (Error::CompilationError (TokenFailureKind::IncompleteError, ..)) =>
        		continue,
    		
    		Err (e) => return Err (e),
    		
    		Ok (_) => return Ok (()),
        }
    };

    /* First, set up the repl environment. */

    /* This function will be run if the user evaluates the symbol `help` */
    scope_table.insert(Value::Atom ("help".to_string()), Value::BuiltinUnaryMethod (|_| {
        println!("{}", REPL_HELP_STRING);
        raise Sys.Break /* Stop executing code. Is this too aggressive? */
    }));
    
    /* This function will be run if the user evaluates the symbol `quit` */
    scope_table.insert(Value::Atom ("quit".to_string()), Value::BuiltinUnaryMethod (|_| {
        raise End_of_file /* ANY attempt to read the "quit" variable quits immediately */
    }));
    
    scope_table.insert(Value::Atom (LAST_KEY_STRING.to_string()), Value::Null);

    /* Next print initial help scroll */
    println!("{}", REPL_HELP_STRING + "\n");

    /* Then run any files provided as arguments */
    run_user_files();

    /* Intercept Control-C so it doesn't kill the REPL. */
    CtrlC::set_handler(|| {
    	println!(""); /* Control-C should clear the line, draw a new prompt */
	});

    try
        /* As long as the user hasn't sent EOF (Control-D), read input */
        loop {
            (try {
                /* Read a full program (throws if program invalid) */
                let buf = prompt_and_read_buffer();

                /* Evaluate program in repl-shared scope */
                let result = execute::execute(starter, buf);

                /* Store final result for next time */
                scope_table.insert(Value::Atom (LAST_KEY_STRING.to_string()), result);

                /* Pretty-print final result */
                println!("{}", pretty::repl_display(result, true));
			}
            with
            if let Err (e) = result
                | Token.CompilationError e ->
                    println!("Error parsing:\n{}", e);
                | Failure e ->
                    println!("Error executing:\n{}", e);
            );

            /* Flush stdout so any output is immediately visible */
            stdout.flush();

            /* Empty lines, since they have all been executed, and repeat */
            lines = vec![];
        }

    with End_of_file ->
        /* Time to exit the REPL */
        println!("Done");
}
