/* This file contains an routine that runs an emily repl off stdin. Once entered, it runs until quit. */
/* In future, this could possibly be moved into its own standalone executable. */

/* Predefined Emily code used by the REPL */
let replHelpString = Options.fullVersion + ", interactive mode
Type \"help\" for help, \"quit\" to quit
Type \"last\" to get the previous line's value"

/* Check if the string 's' ends with a backslash. */
/* FIXME: This is inadequate, the tokenizer should report this itself. */
pub fn isContinued(s) -> bool {
    s.ends_with('\\')
}

lazy_static! {
	pub static ref LAST_KEY_STRING: String = "last".to_string();
}

/* Runs the REPL.

This is the high-level entry point that Main uses to run the REPL.
It needs to set up a global scope to use, then execute the
files provided as arguments (if any) and then start reading
input from the user.

Control-D (EOF) exits the REPL, as does evaluating the word `quit`. */

pub fn repl(target) -> {

    /* This is our global mutable REPL scope. Prepopulate it with some stuff: */
    let starter = loader::complete_starter(LoadLocation::Cwd);
    let scopeTable = value::table_from(starter.root_scope);

    /* Line and lines are used to read and execute user input */
    let mut line = "".to_string();
    let mut lines = vec![];

    /* Display a prompt, then read a line from the user */
    let promptAndReadLine = |s| {
        print!("{}", s);
        flush stdout;
        line = input_line(stdin);
        lines.push(line);
    };

    /* Tokenize and execute the given file target */
    let runFile = |f| {
        let buf = tokenize::tokenize_channel(CodeSource::File(f), open_in(f));
        execute::execute(starter, buf);
    };

    let runString = |s| {
        let buf = tokenize::tokenize_string(CodeSource::Cmdline(s));
        execute::execute(starter, buf);
    };

    /* Run file or -e target */
    let runTargetFile = |t| match t {
        ExecutionTarget::File(f) => { runFile(f); }
        ExecutionTarget::Literal(s) => { runString(s); }
        ExecutionTarget::Stdin => ocaml::failwith("Can't take input from stdin and run in interactive mode at once."),
    };

    /* Load any files provided by the user, before launching REPL */
    let runUserFiles = || if let Some(t) = target { runTargetFile(t) };

    fn promptAndReadBuffer() -> {
        /* Print a prompt, take a line of text, push to "lines" stack */
        promptAndReadLine(match &*lines { [] => ">>> ", _ => "..> " });

        /* Turn the line stack into a "program" */
        let combinedString = lines.iter().cloned().rev().collect::<Vec<_>>().join("\n");

        try
            /* Attempt to parse the program and return an AST. */
            Tokenize.tokenizeString Token.Cmdline combinedString
        with
            /* The program is invalid, but could be valid with more text. Read another line. */
            Token.CompilationError(Token.IncompleteError,_,_) -> promptAndReadBuffer() ) in
    }

    /* First, set up the repl environment. */

    /* This function will be run if the user evaluates the symbol `help` */
    value::tableSetString(scopeTable, "help", Value::BuiltinUnaryMethod |_| {
        println!("{}", replHelpString);
        raise Sys.Break /* Stop executing code. Is this too aggressive? */
    });
    /* This function will be run if the user evaluates the symbol `quit` */
    value::tableSetString(scopeTable, "quit", Value::BuiltinUnaryMethod |_| {
        raise End_of_file /* ANY attempt to read the "quit" variable quits immediately */
    });
    value::tableSetString(scopeTable, lastKeyString, Value::Null);

    /* Next print initial help scroll */
    println!("{}", replHelpString + "\n");

    /* Then run any files provided as arguments */
    runUserFiles();

    /* Intercept Control-C so it doesn't kill the REPL. */
    Sys.catch_break true;

    try
        /* As long as the user hasn't sent EOF (Control-D), read input */
        loop {
            (try
                /* Read a full program (throws if program invalid) */
                let buf = promptAndReadBuffer();

                /* Evaluate program in repl-shared scope */
                let result = execute::execute(starter, buf);

                /* Store final result for next time */
                value::tableSetString(scopeTable, lastKeyString, result);

                /* Pretty-print final result */
                println!("{}", pretty::repl_display(result, true));

            with
                | Sys.Break -> /* Control-C should clear the line, draw a new prompt */
                    print_endline ""
                | Token.CompilationError e ->
                    print_endline @@ "Error parsing:\n" ^ (Token.errorString e)
                | Failure e ->
                    print_endline @@ "Error executing:\n" ^ e
            );

            /* Flush stdout so any output is immediately visible */
            flush stdout;

            /* Empty lines, since they have all been executed, and repeat */
            lines := [];
        }

    with End_of_file ->
        /* Time to exit the REPL */
        print_endline "Done"
