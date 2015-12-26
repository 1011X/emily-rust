/* Functions to create ASTs from strings:

   This is Emily's "parser", which I'm calling a tokenizer since it doesn't do much.
   It scans left to right, categorizing tokens, pushing on left parens and popping on right.
   The parts we'd think of a "parser" as usually doing will be handled in a second,
   currently unimplemented, macro-processing step. */

/* Tokenize uses sedlex which is inherently stateful, so tokenize for a single source string is stateful.
   This is the basic state for a file parse-- it basically just records the position of the last seen newline. */

extern crate regex;

use token::{
	CodePosition,
	CodeSequence,
	Token,
	TokenGroupKind,
	
	CompilationError,
};

struct TokenizeState {
	line_start: isize,
	line: isize
}

enum GroupCloseToken { Eof, Char (String) }

type GroupCloseRecord = (GroupCloseToken, CodePosition);

fn group_close_human_readable(kind: GroupCloseToken) -> String {
	match kind {
		GroupCloseToken::Eof => "end of file".to_string(),
		GroupCloseToken::Char (s) => format!("\"{}\"", s)
	}
}

/* Entry point to tokenize, takes a filename and a lexbuf */
/* TODO: Somehow strip blank lines? */
// Error: CompilationError + Failure
fn tokenize(enclosingKind: TokenGroupKind, name: CodeSource, buf) -> Result<Token, Error> {
    /* -- Helper regexps -- */
    // let digit = Regex::new("[:digit:]").unwrap();
    let number = Regex::new("[:digit:]+").unwrap();
    // let octalDigit = Regex::new("[0-7]").unwrap();
    let octalNumber = Regex::new("0o[0-7]+").unwrap();
    let hexDigit = Regex::new("[:digit:]|[a-fA-F]").unwrap();
    let hexNumber = Regex::new("0x[0-9A-Fa-f]").unwrap();
    let binaryNumber = Regex::new("0b[01]+").unwrap();
    let letterPattern = Regex::new("[:alpha:]").unwrap();
    let wordPattern = Regex::new("[:alpha:][:alnum:]*").unwrap();
    let sciNotation = Regex::new("[eE][-+]?[:digit:]").unwrap();
    let floatPattern = Regex::new(r"\.[:digit:]+|[:digit:]+(?:.+)?").unwrap();
    // let numberPattern = Regex::new(r"").unwrap();
    // TODO: implement
    
    
    /* Helper function: We treat a list as a stack by appending elements to the beginning,
       but this means we have to do a reverse operation to seal the stack at the end. */
    let cleanup = |l| l.clone().reverse();

    /* Individual lines get a more special cleanup so macro processing can occur */
    let complete_line = |l| {
        if options::RUN.step_macro { 
            println!("-- Macro processing for {}", token::file_name_string(name));
        }
        macro_::process(cleanup(l))
    };

    /* -- State management machinery -- */
    /* Current tokenizer state */
    let mut state = TokenizeState {line_start: 0, line: 1};
    /* Call when the current selected sedlex match is a newline. Mutates tokenizer state. */
    let state_newline = || {
        state.line_start = Sedlexing.lexeme_end(buf);
        state.line += 1;
    };
    /* Use tokenizer state to translate sedlex position into a codePosition */
    let current_position = || CodePosition {
        file_name:   name,
        line_number: state.line,
        line_offset: Sedlexing.lexeme_end(buf) - state.line_start
    };
    /* Parse failure. Include current position string. */
    let parse_fail = |mesg| token::fail_at(current_position(), mesg.to_string());
    let incomplete_fail = |mesg| token::incomplete_at(current_position(), mesg.to_string());

    /* -- Parsers -- */

    /* Sub-parser: double-quoted strings. Call after seeing opening quote mark */
    let quoted_string = || {
        /* This parser works by statefully adding chars to a string buffer */
        let mut accum = String::with_capacity(1);
        /* Helper function adds a literal string to the buffer */
        let add = |s| accum.push_str(s);
        /* Helper function adds a sedlex match to the buffer */
        let add_buf = || add(Sedlexing.Utf8.lexeme(buf));
        /* Operate */
        fn proceed() -> Result<> {
            /* Call after seeing a backslash. Matches one character, returns string escape corresponds to. */
            let escaped_char = || match buf {
                '\\' => Ok("\\"),
                '"'  => Ok("\""),
                'n'  => Ok("\n"),
                _    => Err(parse_fail("Unrecognized escape sequence").unwrap_err()) /* TODO: devour newlines */
            };
            /* Chew through until quoted string ends */
            match buf {
                /* Treat a newline like any other char, but since sedlex doesn't track lines, we have to record it */
                '\n' => {
                    state_newline();
                    add_buf();
                    proceed()
                }
                /* Backslash found, trigger escape handler */
                '\\' => {
                	add(escaped_char());
                	proceed()
                }
                /* Unescaped quote found, we are done. Return the data from the buffer. */
                '"' => Ok(accum),
                /* User probably did not intend for string to run to EOF. */
                eof => Err(incomplete_fail("Reached end of file inside string. Missing quote?").unwrap_err()),
                /* Any normal character add to the buffer and proceed. */
                any => {
                	add(Sedlexing.Utf8.lexeme(buf));
                	proceed()
                }
                _ => unreachable!()
            }
        };
        proceed()
    };

    /* Sub-parser: backslash. Eat up to, and possibly including, newline */
    fn escape(seen_text: bool) -> Result<(), CompilationError> {
        let backtrack = || Sedlexing.backtrack(buf);
        match buf {
            /* Have reached newline. We're done. If no command was issued, eat newline. */
            '\n' => Ok(if seen_text { backtrack() } else { state_newline() }),
            /* Skip over white space until newline is reached */
            white_space => escape(seen_text),
            /* A second backslash? Okay, back out and let the main loop handle it */
            '\\' => Ok(backtrack()),
            /* User probably did not intend to concatenate with blank line. */
            eof => if seen_text { Ok(backtrack()) } else {
                Err(incomplete_fail("Found EOF immediately after backslash, expected token or new line.").unwrap_err());
            },
            /* TODO: Ignore rather than error */
            any => Err(parse_fail("Did not recognize text after backslash.").unwrap_err()),
            _ => unreachable!()
        }
    }

    /* Main loop. */
    /* Takes a constructor that's prepped with all the properties for the enclosing group, and
       needs only the final list of lines to produce a token. Returns a completed group. */
    
    // Error: CompilationError + Failure
    fn proceed<F>(group_close: GroupCloseRecord, group_seed: F, lines: Vec<>, line: Vec<>) -> Result<Token, Error> where
    F: Fn(Vec<Token>, CodeSequence) -> Token {
        /* Constructor for a token with the current preserved codeposition. */
        let make_token_here = token::make_token(current_position());

        /* Right now all group closers are treated as equivalent. TODO: Don't do it like this. */
        let close_pattern = [%sedlex.regexp? '}' | ')' | ']' | eof];

        /* Recurse with the same groupSeed we started with. */
        let proceed_with_lines = |ls, l| proceed(group_close, group_seed, ls, l);

        /* Recurse with the same groupSeed and lines we started with. */
        let proceed_with_line = |l| proceed_with_lines(lines, l);

        /* Recurse with all the same arguments  we started with. */
        let skip = || proceed_with_line(line);

        /* Helper function: Get current sedlex match */
        let matched_lexemes = || Sedlexing.Utf8.lexeme(buf);

        /* Complete current line & push onto current codeSequence */
        let lines_plus_line = || {
        	let mut l = lines.clone();
        	l.push(complete_line(line));
        	l
        };

        /* Recurse with the groupSeed and lines we started with, & the argument pushed onto the current line */
        /* Notice stateNewLine and add_to_line_proceed are using ndifferent notions of a "line". */
        let add_to_line_proceed = |x| proceed_with_line({
        	let mut l = line.clone();
        	l.push(x);
        	l
        });

        /* Recurse with the groupSeed we started with, the current line pushed onto the codeSequence, & a new blank line */
        let new_line_proceed = || proceed_with_lines(lines_plus_line(), vec![]);

        /* Complete processing the current group by completing the current codeSequence & feeding it to the groupSeed. */
        let close_group = |cs| group_seed(cleanup(lines_plus_line()), cs);

        /* Helper: Given a string->tokenContents mapping, make the token, add it to the line and recurse */
        fn add_single<F: Fn(String) -> TokenContents>(constructor: F) -> {
        	add_to_line_proceed(make_token_here(constructor(matched_lexemes())))
        }

        let group_close_make_from = |st| {
            let ch = match st {
                "{" => "}",
                "(" => ")",
                "[" => "]",
                _ => return Err(parse_fail("Internal failure: interpreter is confused about parenthesis").unwrap_err())
            };
            
            Ok((GroupCloseToken::Char (ch), current_position()))
        };

        let group_close_under_token = || match matched_lexemes() {
        	"" => GroupCloseToken::Eof,
        	s => GroupCloseToken::Char (s)
        };

        /* Recurse with blank code, and a new group_seed described by the arguments */
        fn open_group(closure_kind: TokenClosureKind, group_kind: TokenGroupKind) ->  {
            proceed(
            	group_close_make_from(matched_lexemes()),
                |l, cs| token::make_group(&current_position(), &closure_kind, &group_kind, l, cs),
                vec![],
                vec![]
            )
        }

        /* Variant assuming non-closure */
        let open_ordinary_group = open_group(TokenClosureKind::NonClosure);

        /* Now finally here's the actual grammar... */
        match buf {
            /* Ignore comments */
            ('#', Star (Compl '\n')) => skip(),

            /* On ANY group-close symbol, we end the current group */
            closePattern => {
                /* However, we have to check to make sure that the symbol matches */
                let candidate_close = group_close_under_token();
                /* The expected group close comes packed with the position of the opening symbol */
                let (group_close, group_close_at) = group_close;
                /* This is a matching close */
                if candidate_close == group_close { Ok(close_group()) }
                /* This is not a matching close */
                else {
                	match candidate_close {
		                /* No close before EOF: Failure is positioned at the opening symbol */
		                GroupCloseToken::Eof => Err(token::incomplete_at(group_close_at,
		                    &format!("Did not find matching {} anywhere before end of file. Opening symbol:"), group_close_human_readable(group_close)).unwrap_err())
		                /* Close present, but wrong: Failure is positioned at closing symbol */
		                GroupCloseToken::Char (_) => Err(parse_fail(
                        format!("Expected closing {} but instead found {}", group_close_human_readable(group_close), group_close_human_readable(candidate_close))).unwrap_err())
                    }
                }
            }

            /* Quoted string */
            '"' => add_to_line_proceed(make_token_here(TokenContents::String (quoted_string()))),

            /* Floating point number */
            floatPattern => add_single(|x| TokenContents::Number (try!(x.parse::<f64>()))),

            /* Local scope variable */
            wordPattern => add_single(|x| TokenContents::Word (x)),

            /* Line demarcator */
            ';' => new_line_proceed(),

            /* Line demarcator (but remember, we have to track newlines) */
            '\n' => { state_newline(); new_line_proceed() }

            /* Reader instructions.
               TODO: A more general system for reader instructions; allow tab after \version */
            "\\version 0.1" => { escape(true); skip() } /* Ignore to end of line, don't consume */
            "\\version 0.2" => { escape(true); skip() } /* Ignore to end of line, don't consume */
            '\\' => { escape(false); skip() }             /* Ignore to end of line and consume it */

            /* Ignore whitespace */
            white_space => skip(),

            /* On groups or closures, open a new parser (NO TCO AT THIS TIME) and add its result token to the current line */
            '(' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Plain)),
            '{' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Scoped)),
            '[' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Box (BoxKind::NewObject))),
            Plus(Compl(Chars "#()[]{}\\;\""|digit|letterPattern|white_space))
               => add_single(|x| TokenContents::Symbol (x)),
            _ => Err(parse_fail("Unexpected character").unwrap_err()) /* Probably not possible? */
        }
    }

    /* When first entering the parser, treat the entire program as implicitly being surrounded by parenthesis */
    proceed((GroupCloseToken::Eof, current_position()), token::make_group(current_position()), TokenClosureKind::NonClosure (enclosing_kind), vec![], vec![])
}

/* Tokenize entry point typed to channel */
pub fn tokenize_channel<C: Read>(source: CodeSource, channel: C) -> Result<Token, Error> {
    let lexbuf = Sedlexing.Utf8.from_channel(channel);
    tokenize(TokenGroupKind::Plain, source, lexbuf)
}

/* Tokenize entry point typed to string */
pub fn tokenize_string(source: CodeSource, string: String) -> Result<Token, Error> {
    let lexbuf = Sedlexing.Utf8.from_string(string);
    tokenize(TokenGroupKind::Plain, source, lexbuf)
}

fn unwrap(token: Token) -> Result<CodeSequence, String> {
	match token.contents {
		TokenContents::Group (g) => Ok (g.items),
		_ => Err (format!("Internal error: Object in wrong place {}", token::position_string(token.at)))
	}
}

fn snippet(source: CodeSource, st: String) -> Result<CodeSequence, String> {
    match tokenize_string(source, st) {
        Ok (v) => unwrap(v),
        
        Err (CompilationError (e)) => Err(format!(
            "Internal error: Interpreter-internal code is invalid:{}",
            token::error_string(e)))
    }
}
