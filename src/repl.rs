/* This file contains an routine that runs an emily repl off stdin. Once entered, it runs until quit. */
/* In future, this could possibly be moved into its own standalone executable. */

use std::io::{self, prelude::*};
use lazy_static::lazy_static;

//use crate::execute;
//use crate::loader::{self, *};
use crate::options;
use crate::pretty;
use crate::token::*;
//use crate::tokenize;
use crate::value::*;

/* Predefined Emily code used by the REPL */
lazy_static! {
    pub static ref REPL_HELP_STRING: String = *options::FULL_VERSION + r#", interactive mode\
    Type "help" for help, "quit" to quit\
    Type "last" to get the previous line's value"#;
}

/* Check if the string 's' ends with a backslash. */
/* FIXME: This is inadequate, the tokenizer should report this itself. */
fn is_continued(s: &str) -> bool {
    !s.is_empty() && s.ends_with('\\')
}

pub static LAST_KEY_STRING: &str = "last";

/* Runs the REPL.

This is the high-level entry point that Main uses to run the REPL.
It needs to set up a global scope to use, then execute the
files provided as arguments (if any) and then start reading
input from the user.

Control-D (EOF) exits the REPL, as does evaluating the word `quit`. */
pub fn repl(target: Option<ExecutionTarget>) {
    /* This is our global mutable REPL scope. Prepopulate it with some stuff: */
    let starter = loader::complete_starter(LoadLocation::Cwd);
    let scope_table = TableValue::from(starter.root_scope);
    
    /* Line and lines are used to read and execute user input */
    let line = String::new();
    let lines = Vec::new();

    /* First, set up the repl environment. */
    
    /* This function will be run if the user evaluates the symbol `help` */
    scope_table.insert(Value::from_atom("help"), Value::BuiltinUnaryMethod(|_| {
        println!("{}", *REPL_HELP_STRING);
        // TODO: these
        //raise Sys.Break /* Stop executing code. Is this too aggressive? */
    }));
    /* This function will be run if the user evaluates the symbol `quit` */
    scope_table.insert(Value::from_atom("quit"), Value::BuiltinUnaryMethod(|_| {
        //raise End_of_file /* ANY attempt to read the "quit" variable quits immediately */
    }));
    scope_table.insert(Value::from_atom(LAST_KEY_STRING), Value::Null);
    
    /* Next print initial help scroll */
    println!("{}\n", *REPL_HELP_STRING);
    
    /* Then run any files provided by the user as arguments */
    if let Some(t) = target {
        /* Run file or -e target */
        match t {
            /* Tokenize and execute the given file target */
            ExecutionTarget::File(f) => {
                let buf = tokenize::tokenize_channel(CodeSource::File(f), open_in(f));
                execute::execute(starter, buf);
            }
            ExecutionTarget::Literal(s) => {
                let buf = tokenize::tokenize_string(CodeSource::Cmdline, s);
                execute::execute(starter, buf);
            }
            ExecutionTarget::Stdin => panic!("Can't take input from stdin and run in interactive mode at once."),
        }
    }
    
    /* Intercept Control-C so it doesn't kill the REPL. */
    ctrlc::set_handler(|| println!()).unwrap();
    
    /* As long as the user hasn't sent EOF (Control-D), read input */
    'end: loop {
        /* Read a full program (throws if program invalid) */
        let input = loop {
            /* Print a prompt, take a line of text, push to "lines" stack */
            print!("{}", match *lines { [] => ">>> ", _ => "..> " });
            io::stdout().flush().unwrap();
            let bytes_read = io::stdin().read_line(&mut line).unwrap();
            // reached EOF
            if bytes_read == 0 && line.is_empty() {
                break 'end;
            }
            lines.push(line.clone());
            
            /* Turn the line stack into a "program" */
            let combined_string = lines.join("\n");
            
            /* Attempt to parse the program and return an AST. */
            match tokenize::tokenize_string(CodeSource::Cmdline, &combined_string) {
                Ok(res) => break Ok(res),
                /* The program is invalid, but could be valid with more text. Read another line. */
                Err(CompilationError(TokenFailureKind::IncompleteError, _, _)) => continue,
                Err(e) => break Err(e)
            }
        };
        
        let buf = match input {
            Ok(buf) => buf,
            Err(e) => println!("Error parsing:\n{}", e),
        };
        
        /* Evaluate program in repl-shared scope */
        let result = match execute::execute(starter, buf) {
            Ok(res) => res,
            Err(e) => println!("Error executing:\n{}", e),
        };
        
        /* Store final result for next time */
        scope_table.insert(Value::from_atom(LAST_KEY_STRING), result);
        
        /* Pretty-print final result */
        println!("{}", pretty::repl_display(result, true));
        
        /* Flush stdout so any output is immediately visible */
        io::stdout().flush().unwrap();
        
        /* Empty lines, since they have all been executed, and repeat */
        lines.clear();
    }
    
    /* Time to exit the REPL */
    println!("Done");
}
