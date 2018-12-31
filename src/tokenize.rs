/* Functions to create ASTs from strings:

   This is Emily's "parser", which I'm calling a tokenizer since it doesn't do much.
   It scans left to right, categorizing tokens, pushing on left parens and popping on right.
   The parts we'd think of a "parser" as usually doing will be handled in a second,
   currently unimplemented, macro-processing step. */

use lazy_static::lazy_static;
use regex::{Regex, RegexSet};

use crate::token;
//use crate::macros;

/* Tokenize uses sedlex which is inherently stateful, so tokenize for a single source string is stateful.
   This is the basic state for a file parse-- it basically just records the position of the last seen newline. */
struct TokenizeState {
    line_start: usize,
    line: usize,
}

enum GroupCloseToken {
    Eof,
    Char(String),
}
type GroupCloseRecord = (GroupCloseToken, token::CodePosition);
//type groupCloseRecord = groupCloseToken*Token.codePosition

// groupCloseHumanReadable
impl fmt::Display for GroupCloseToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GroupCloseToken::Eof         => f.write_str("end of file"),
            GroupCloseToken::Char(ref s) => write!(f, "\"{}\"", s),
        }
    }
}

/* Entry point to tokenize, takes a filename and a lexbuf */
/* TODO: Somehow strip blank lines? */
let tokenize enclosingKind name (mut buf:std::str::Chars) : Token.token =
    /* -- Helper regexps -- */
    lazy_static! {
        //let digit = [%sedlex.regexp? '0'..'9'] in
        static ref NUMBER: Regex = Regex::new("^[0-9]+").unwrap();
        //let octalDigit = [%sedlex.regexp? '0'..'7'] in
        //let octalNumber = [%sedlex.regexp? "0o", Plus octalDigit] in
        //let hexDigit = [%sedlex.regexp? '0'..'9'|'a'..'f'|'A'..'F'] in
        //let hexNumber = [%sedlex.regexp? "0x", Plus hexDigit] in
        //let binaryNumber = [%sedlex.regexp? "0b", Plus ('0'|'1') ] in
        static ref LETTER_PATTERN: Regex = Regex::new("^[a-zA-Z]");
        static ref WORD_PATTERN: Regex = Regex::new("^[a-zA-Z][a-zA-Z0-9_]*");
        //let sciNotation = [%sedlex.regexp? ('e'|'E'), Opt('+'|'-'), number ] in
        //let floatPattern = [%sedlex.regexp? '.',number | number, Opt('.', number), Opt sciNotation] in
        static ref NUMBER_PATTERN: RegexSet = RegexSet::new(&[
            "^0x[0-9a-fA-F]+",
            "^0o[0-7]+",
            r"^\.\d+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?",
            "^0b[01]+",
        ]);
    }

    /* Helper function: We treat a list as a stack by appending elements to the beginning,
       but this means we have to do a reverse operation to seal the stack at the end. */
    // XXX likely not needed
    //let cleanup = List.rev in

    /* Individual lines get a more special cleanup so macro processing can occur */
    let complete_line = |l| {
        if options::RUN.read().unwrap().step_macro {
            println!("-- Macro processing for {}", token::file_name_string(name));
        }
        //macros::process(cleanup(l));
        macros::process(l);
    };

    /* -- State management machinery -- */
    /* Current tokenizer state */
    let mut state = TokenState {line_start: 0, line: 1};
    /* Call when the current selected sedlex match is a newline. Mutates tokenizer state. */
    let state_newline = || {
        state.line_start = 0;
        state.line += 1;
    };
    /* Use tokenizer state to translate sedlex position into a codePosition */
    let current_position = || token::CodePosition {
        file_name: name,
        line_number: state.line,
        line_offset: Sedlexing::lexeme_end(buf) - state.line_start,
    };
    /* Parse failure. Include current position string. */
    let parse_fail = |mesg| token::fail_at(current_position(), mesg);
    let incomplete_fail = |mesg| token::incomplete_at(current_position(), mesg);

    /* -- Parsers -- */

    /* Sub-parser: backslash. Eat up to, and possibly including, newline */
    let escape = |seen_text| loop {
        let mut bt = buf.clone();
        let backtrack = || buf = bt;
        
        match buf.next() {
            /* Have reached newline. We're done. If no command was issued, eat newline. */
            Some('\n') => {
                if seen_text { backtrack() } else { state_newline() }
                return Ok(());
            }
            
            /* Skip over white space until newline is reached */
            Some(c) if c.is_whitespace() => {}
            
            /* A second backslash? Okay, back out and let the main loop handle it */
            Some('\\') => {
                backtrack();
                return Ok(());
            }
            
            /* Comments are allowed after a backslash, and ignored as normal. */
            Some('#') => {
                while buf.next() != Some('\n') {
                    bt = buf.clone();
                }
                backtrack();
            }
            
            /* User probably did not intend to concatenate with blank line. */
            None =>
                if seen_text {
                    backtrack();
                    return Ok(());
                }
                else {
                    return Err(incomplete_fail("Found EOF immediately after backslash, expected token or new line."));
                }
            
            /* TODO: Ignore rather than error */
            Some(_any) =>
                return Err(parse_fail("Did not recognize text after backslash."))
        }
    };
    
    /* Main loop. */
    /* Takes a constructor that's prepped with all the properties for the enclosing group, and
       needs only the final list of lines to produce a token. Returns a completed group. */
    let proceed = |group_close, group_seed, group_init, lines, line| {
        /* Constructor for a token with the current preserved codeposition. */
        let make_token_here = |contents| Token::new(current_position(), contents);

        /* Recurse with the same groupSeed we started with. */
        let proceed_with_initializer = |gi, ls, l| proceed(group_close, group_seed, gi, ls, l);
        
        /* Recurse with the same groupSeed and initializer we started with. */
        let proceed_with_lines = |ls, l| proceed_with_initializer(group_initializer, ls, l);
        
        /* Recurse with the same groupSeed, initializer and lines we started with. */
        let proceed_with_line = |l| proceed_with_lines(lines, l);
        
        /* Recurse with all the same arguments we started with. */
        let skip = || proceed_with_line(line);
        
        /* Helper function: Get current sedlex match */
        // TODO
        let matched_lexemes = || Sedlexing.Utf8.lexeme(buf) in

        /* Complete current line & push onto current codeSequence */
        let lines_plus_line = || {
            let mut l = lines.clone();
            l.push(complete_line(line));
            l
        };

        /* Recurse with the groupSeed and lines we started with, & the argument pushed onto the current line */
        /* Notice stateNewLine and addToLineProceed are using ndifferent notions of a "line". */
        let add_to_line_proceed = |x| {
            let mut l = line.clone();
            l.push(x);
            proceed_with_line(l)
        };

        /* Recurse with the groupSeed we started with, the current line pushed onto the codeSequence, & a new blank line */
        let new_line_proceed = |x| proceed_with_lines(lines_plus_line(), vec![]);

        /* Complete processing the current group by completing the current codeSequence & feeding it to the groupSeed. */
        let close_group = || group_seed(group_initializer, cleanup(lines_plus_line()));

        /* Helper: Given a string->tokenContents mapping, make the token, add it to the line and recurse */
        let add_single = |constructor| add_to_line_proceed(make_token_here(constructor(matched_lexemes())));

        /* Helper: Function-ized Symbol constructor */
        //let make_symbol = |x| TokenContents::Symbol(x);

        /* Helper: See if lines contains anything substantial */
        let any_nonblank = |x|  !x.iter().all(|e| e.is_empty());

        let group_close_make_from = |s| {
            let c = match s {
                "{" => "}",
                "(" => ")",
                "[" => "]",
                _ => Err(parse_fail("interpreter is confused about parenthesis"))
            };
            (GroupCloseToken::Char(c), current_position())
        };
        
        let group_close_under_token = || match matched_lexemes() {
            "" => GroupCloseToken::Eof,
            s => GroupCloseToken::Char(s)
        };

        /* Recurse with blank code, and a new groupSeed described by the arguments */
        let open_group = |closure_kind, group_kind|
            proceed(group_close_make_from(matched_lexemes()),
                token::make_group(current_position(), closure_kind, group_kind), vec![], vec![], vec![]);

        /* Variant assuming non-closure */
        let open_ordinary_group = |kind| open_group(TokenClosureKind::NonClosure, kind);

        /* Now finally here's the actual grammar... */
        match buf.next() {
            /* Ignore comments */
            Some('#') => {
                while buf.next() != Some('\n') {}
                skip();
            }

            /* Right now all group closers are treated as equivalent. TODO: Don't do it like this. */
            /* On ANY group-close symbol, we end the current group */
            Some('}') | Some(')') | Some(']') | None => {
                /* However, we have to check to make sure that the symbol matches */
                let candidate_close = group_close_under_token();
                /* The expected group close comes packed with the position of the opening symbol */
                let (group_close, group_close_at) = group_close;
                /* This is a matching close */
                if candidate_close == group_close { close_group() }
                /* This is not a matching close */
                else {
                    match candidate_close {
                        /* No close before EOF: Failure is positioned at the opening symbol */
                        GroupCloseToken::Eof => token::incomplete_at(group_close_at,
                            &format!("Did not find matching {} anywhere before end of file. Opening symbol:", group_close)),
                        /* Close present, but wrong: Failure is positioned at closing symbol */
                        GroupCloseToken::Char(_) => parse_fail(
                            &format!("Expected closing {} but instead found {}", groupClose, candidateClose)
                        ),
                    }
                }
            }

            /* Quoted string */
            Some('"') => add_to_line_proceed(make_token_here(TokenContents::String({
                /* This parser works by statefully adding chars to a string buffer */
                let mut b = buf.chars();
                let mut accum = String::new();
                
                loop {
                    /* Chew through until quoted string ends */
                    match b.next() {
                        /* Treat a newline like any other char, but since sedlex doesn't track lines, we have to record it */
                        Some('\n') => {
                            state_newline();
                            accum.push('\n');
                        }
                        /* Backslash found, trigger escape handler */
                        /* Matches one character, returns string escape it corresponds to. */
                        Some('\\') => accum += match buf.next() {
                            Some('\\') => "\\",
                            Some('"')  => "\"",
                            Some('n')  => "\n",
                            _ => break Err(parse_fail("Unrecognized escape sequence")) /* TODO: devour newlines */
                        },
                        /* Unescaped quote found, we are done. Return the data from the buffer. */
                        Some('"') => break Ok(accum),
                        /* User probably did not intend for string to run to EOF. */
                        None => break Err(incomplete_fail("Reached end of file inside string. Missing quote?")),
                        /* Any normal character add to the buffer and proceed. */
                        Some(any) => accum.push(any)
                    }
                }?
            }))),

            /* Floating point number
               More complicated because float_of_string doesn't handle octal or binary */
            | number_pattern -> {
                add_single(|x|
                    if x.len() < 3 {
                        TokenContents::Number(x.parse().unwrap())
                    } else {
                        match &x[0..2] {
                            "0b" => TokenContents::Number(i32::from_str_radix(x, 2) as f64)
                            "0o" => TokenContents::Number(i32::from_str_radix(x, 8) as f64)
                            | _         -> Token.Number(float_of_string x)
                        }
                    }
                )
            Some('0') => {
                add_single(|x| match &x[0..2] {
                    
                })
            }

            /* Local scope variable */
            | wordPattern -> addSingle (fun x -> Token.Word x)

            /* Line demarcator */
            | ';' -> proceed groupClose groupSeed groupInitializer (linesPlusLine()) []

            /* Line demarcator (but remember, we have to track newlines) */
            | '\n' -> stateNewline(); proceed groupClose groupSeed groupInitializer (linesPlusLine()) []

            /* Reader instructions.
               TODO: A more general system for reader instructions; allow tab after \version */
            | "\\version 0.1"  -> escape true; skip() /* Ignore to end of line, don't consume */
            | "\\version 0.2"  -> escape true; skip() /* Ignore to end of line, don't consume */
            | "\\version 0.3b"  -> escape true; skip() /* Ignore to end of line, don't consume */
            | '\\' -> escape false; skip()            /* Ignore to end of line and consume it */

            /* Ignore whitespace */
            | white_space -> skip ()

            /* On groups or closures, open a new parser (NO TCO AT THIS TIME) and add its result token to the current line */
            | '(' -> addToLineProceed( openOrdinaryGroup Token.Plain )
            | '{' -> addToLineProceed( openOrdinaryGroup Token.Scoped )
            | '[' -> addToLineProceed( openOrdinaryGroup @@ Token.Box Token.NewObject )

            /* If a | is seen, this demarcates the initializer */
            | '|' -> if any_nonblank(&lines)
                { addSingle TokenContents::Symbol } /* If we've passed our chance for an initializer, this is just a pipe */
                else proceed groupClose groupSeed ( /* Otherwise it's an initializer */
                    if Options.(run.stepMacro) then print_endline @@ "-- Macro processing for initializer in "^(Token.fileNameString name);
                    Macro.process @@ cleanup line
                ) lines [] /* Otherwise swap line into the initializer spot */

            | Plus(Compl(Chars "#()[]{}\\;\""|digit|letterPattern|white_space))
                -> addSingle TokenContents::Symbol
            | _ -> parseFail "Unexpected character" /* Probably not possible? */
        }
    /* When first entering the parser, treat the entire program as implicitly being surrounded by parenthesis */
    in proceed (Eof,currentPosition()) (Token.makeGroup (currentPosition()) Token.NonClosure enclosingKind) [] [] []
    };

/* Tokenize entry point typed to channel */
let tokenizeChannel source channel =
    let lexbuf = Sedlexing.Utf8.from_channel channel in
    tokenize Token.Plain source lexbuf

/* Tokenize entry point typed to string */
let tokenizeString source str =
    let lexbuf = Sedlexing.Utf8.from_string str in
    tokenize Token.Plain source lexbuf

fn unwrap(token: Token) -> CodeSequence {
    match token.contents {
        TokenContents::Group(g) => g.items,
        _ => panic!("Internal error: Object in wrong place {}", token::position_string(token.at)))
    }
}
/*
let unwrap token = match token.Token.contents with
    | Token.Group g -> g.Token.items
    | _ -> failwith(Printf.sprintf "%s %s" "Internal error: Object in wrong place" (Token.positionString token.Token.at))
*/

fn snippet(source: CodeSource, s: String) -> Result<CodeSequence, String> {
    unwrap(match tokenize_string(source, s) {
        Ok(r) => r,
        Err(err) => return Err(format!(
            "Internal error: Interpreter-internal code is invalid: {}",
            err
        )),
    })
}
/*
let snippet source str =
    try
        unwrap @@ tokenizeString source str
    with
        Token.CompilationError e -> failwith @@
            "Internal error: Interpreter-internal code is invalid:" ^
            (Token.errorString e)
*/
