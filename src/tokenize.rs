/* Functions to create ASTs from strings:

   This is Emily's "parser", which I'm calling a tokenizer since it doesn't do much.
   It scans left to right, categorizing tokens, pushing on left parens and popping on right.
   The parts we'd think of a "parser" as usually doing will be handled in a second,
   currently unimplemented, macro-processing step. */

// May as well start from scratch with nom?

use std::io;
use std::result;
use std::borrow::Cow;

use nom;

use macros;
use options;
// is there a reason not to use ::* ?
use token::{
    self,
    CodeSource,
    CodePosition,
    CodeSequence,
    Token,
    TokenGroupKind,
    TokenContents,
    TokenClosureKind,
    BoxKind,
    
    CompilationError,
};

pub enum Error {
    Compilation(token::CompilationError),
    Failure(String),
    Io(io::Error),
}

/* Tokenize uses sedlex which is inherently stateful, so tokenize for a single source string is stateful.
   This is the basic state for a file parse-- it basically just records the position of the last seen newline. */

use std::fmt;
use token;

#[derive(Clone)]
struct TokenizeState {
	line_start: u32,
	line: u32,
}

enum GroupCloseToken {
	Eof,
	Char(String),
}

/* Entry point to tokenize, takes a filename and a lexbuf */
/* TODO: Somehow strip blank lines? */
pub fn tokenize(enclosing_kind: TokenGroupKind, name: CodeSource, mut buf: String) -> Result<Token, Error> {
    /* -- Helper regexps -- */
    named!(float_pattern, re_bytes_find!(r"^(\.[:digit:]+|[:digit:]+(\.[:digit:])?([eE][-+]?[:digit:]+)?)"));
    
    named!(octal_number, preceded!(tag!("0o"), oct_digit));
    named!(hex_number, preceded!(tag!("0x"), hex_digit));
    named!(binary_number, preceded!(tag!("0b"), is_a!("01")));
    named!(word_pattern, re_bytes_find!("^[:alpha:][:alnum:]*"));
    /*
    named!(number_pattern, alt!(
    	hex_number | octal_number | float_pattern | binary_number
    ));
    */
    named!(number_pattern<f64>, alt!(
        map!(
        	map_res!(hex_number, apply!(u32::from_str_radix, 16)),
        	|int| int as f64
    	)
        | map!(
        	map_res!(binary_number, apply!(u32::from_str_radix, 2)),
        	|int| int as f64
    	)
        | map!(
        	map_res!(octal_number, apply!(u32::from_str_radix, 8)),
        	|int| int as f64
    	)
    	// might not work because it returns a ParseFloatError while
    	// the rest return a ParseIntError
        | map_res!(float_pattern, |i| i.parse())
    ));
    
    named!(comment, re_bytes_find!("^#[^\n]*"));
    
    /* Helper function: We treat a list as a stack by appending elements to the beginning,
       but this means we have to do a reverse operation to seal the stack at the end. */
    // XXX: should never run because vectors append elements at the end rather than the
    // beginning.
    /*
    let cleanup = |l| {
        let lt = l.clone();
        lt.reverse();
        lt
    };
    */

    /* Individual lines get a more special cleanup so macro processing can occur */
    let complete_line = |l| {
        if options::RUN.read().unwrap().step_macro {
            println!("-- Macro processing for {}", name);
        }
        
        macros::process(l)
    };

    /* -- State management machinery -- */
    /* Current tokenizer state */
    let mut state = TokenizeState {
        line_start: 0,
        line: 1,
    };
    /* Call when the current selected sedlex match is a newline. Mutates tokenizer state. */
    let state_newline = || {
        state.line_start = Sedlexing.lexeme_end(buf);
        state.line += 1;
    };
    /* Use tokenizer state to translate sedlex position into a CodePosition */
    let current_position = || CodePosition {
        file_name:   name,
        line_number: state.line,
        line_offset: Sedlexing.lexeme_end(buf) - state.line_start,
    };
    /* Parse failure. Include current position string. */
    let parse_fail = |mesg| token::fail_at(&current_position(), mesg);
    let incomplete_fail = |mesg| token::incomplete_at(&current_position(), mesg);

    /* -- Parsers -- */
    
    /* Sub-parser: double-quoted strings. Call after seeing opening quote mark */
    // double-quoted string. escapes backslashes, double-quotes, and
    // newlines. must error on unrecognized escape sequence, and on
    // unexpected eof.
    let quoted_string = closure!(delimited!(
        tag!("\""),
        map_res!(
            escaped_transform!(is_not!("\\"), '\\', alt_complete!(
                value!(b"\\", tag!("\\"))
                | value!(b"\"", tag!("\""))
                | tap!(_: value!(b"\n", tag!("n")) => {
                	state_newline();
            	})
            	// Error: unrecognized escape sequence
            	// Error: reached end of file inside string. Missing quote?
                /* TODO: devour newlines */
            )),
            String::from_utf8
        ),
        tag!("\"")
    ));
    
    // Just in case:
    /* XXX
    let quoted_string = || {
        /* This parser works by statefully adding chars to a string buffer */
        let mut accum = String::new();
        
        /* Operate */
        let proceed = || {
            /* Call after seeing a backslash. Matches one character, returns string escape corresponds to. */
            let escaped_char = |c| match c {
                '\\' => Ok("\\"),
                '"'  => Ok("\""),
                'n'  => Ok("\n"),
                _    => Err(parse_fail("Unrecognized escape sequence")) /* TODO: devour newlines */
            };
            
            /* Chew through until quoted string ends */
            let chars = buf.chars();
            loop {
                match chars.next() {
                    /* Treat a newline like any other char, but since sedlex doesn't track lines, we have to record it */
                    Some(c @ '\n') => {
                        state_newline();
                        accum.push(c);
                    }
                    
                    /* Backslash found, trigger escape handler */
                    Some(c @ '\\') => {
                        // FIXME: handle unwrap()
                        let result = escaped_char(chars.next().unwrap());
                        accum.push(result?);
                    }
                    
                    /* Unescaped quote found, we are done. Return the data from the buffer. */
                    Some('"') => break,
                    
                    /* User probably did not intend for string to run to EOF. */
                    None =>
                        Err(incomplete_fail("Reached end of file inside string. Missing quote?")),
                    
                    /* Any normal character add to the buffer and proceed. */
                    Some(c) => accum.push(c)
                }
            }
            
            Ok(accum)
        };
        
        proceed()
    };
    XXX */

    /* Sub-parser: backslash. Eat up to, and possibly including, newline */
    let escape = |seen_text| {
        let backtrack = || Sedlexing.backtrack(buf);
        loop {match buf {
            /* Have reached newline. We're done. If no command was issued, eat newline. */
            if buf.starts_with('\n') {
                if seen_text {
                    return Ok(backtrack());
                } else {
                    return Ok(state_newline());
                }
            }
            /* Skip over white space until newline is reached */
            else if buf.starts_with(char::is_whitespace) {
                buf = buf.trim_left();
            }
            /* A second backslash? Okay, back out and let the main loop handle it */
            else if buf.starts_with('\\') {
                return Ok(backtrack());
            }
            /* Comments are allowed after a backslash, and ignored as normal. */
            else if buf[..1] == "#" && buf[1..].chars().all(|c| c != '\n') {}
            /* User probably did not intend to concatenate with blank line. */
            eof =>
                if seen_text {
                    Ok(backtrack())
                } else {
                    Err(incomplete_fail("Found EOF immediately after backslash, expected token or new line."))
                },
            /* TODO: Ignore rather than error */
            any => Err(parse_fail("Did not recognize text after backslash.")),
            _ => unreachable!()
        }}
    };

    /* Main loop. */
    /* Takes a constructor that's prepped with all the properties for the enclosing group, and
       needs only the final list of lines to produce a token. Returns a completed group. */
    
    //proceed<F>(GroupCloseRecord, F, Vec<Token>, Vec<Vec<Token>>, Vec<Token>) -> Result<Token, Error> where
    //F: Fn(Vec<Token>, CodeSequence) -> Token {
    let proceed = |group_close, group_seed, mut group_initializer, mut lines, mut line| {
        /* Constructor for a token with the current preserved codeposition. */
        let make_token_here = |contents| Token::new(current_position(), contents);

        /* Right now all group closers are treated as equivalent. TODO: Don't do it like this. */
        let close_pattern = closure!(re_bytes_find!(r"^[})\]$]"));
        
        /* Recurse with the same groupSeed we started with. */
        let proceed_with_initializer = |init, ls, l| {
            group_initializer = init;
            lines = ls;
            line = l;
        };
        
        /* Recurse with the same groupSeed and initializer we started with. */
        let proceed_with_lines = |ls, l| {
            lines = ls;
            line = l;
        };
        
        /* Recurse with the same groupSeed and lines we started with. */
        let proceed_with_line = |l| line = l;

        /* Helper function: Get current sedlex match */
        let matched_lexemes = || Sedlexing.Utf8.lexeme(buf);

        /* Complete current line & push onto current codeSequence */
        let lines_plus_line = || {
            lines.push(line);
            complete_line(lines)
        };

        /* Recurse with the group_seed and lines we started with, & the argument pushed onto the current line */
        /* Notice state_new_line and add_to_line_proceed are using different notions of a "line". */
        let add_to_line_proceed = |x| {line.push(x); line};

        /* Recurse with the group_seed we started with, the current line pushed onto the CodeSequence, & a new blank line */
        let new_line_proceed = || proceed_with_lines(lines_plus_line(), vec![]);

        /* Complete processing the current group by completing the current CodeSequence & feeding it to the group_seed. */
        let close_group = || group_seed(group_initializer, lines_plus_line());

        /* Helper: Given a String -> TokenContents mapping, make the token, add it to the line and recurse */
        let add_single = |constructor| {
            add_to_line_proceed(make_token_here(constructor(matched_lexemes())))
        };
        
        /* Helper: Function-ized Symbol constructor */
        let make_symbol = TokenContents::Symbol;
        
        /* Helper: See if lines contains anything substantial */
        let any_nonblank = |mut x| loop { match *x {
            [] => return false,
            [l, ref more..] if l.is_empty() => x = more.to_vec(),
            _ => return true
        }};
        
        let group_close_make_from = |st| {
            let ch = match st {
                "{" => "}",
                "(" => ")",
                "[" => "]",
                _ => return Err(parse_fail("Internal failure: interpreter is confused about parenthesis"))
            };
            
            Ok((GroupCloseToken::Char(ch), current_position()))
        };

        let group_close_under_token = || match matched_lexemes() {
            "" => GroupCloseToken::Eof,
            s => GroupCloseToken::Char(s),
        };

        /* Recurse with blank code, and a new group_seed described by the arguments */
        //fn open_group(closure_kind: TokenClosureKind, group_kind: TokenGroupKind) -> Result<Token, Error> {
        let open_group = |closure_kind, group_kind| {
            proceed(
                group_close_make_from(matched_lexemes()),
                |l, cs| token::make_group(&current_position(), &closure_kind, &group_kind, l, cs),
                vec![],
                vec![],
                vec![]
            )
        };

        /* Variant assuming non-closure */
        let open_ordinary_group = |gk| open_group(TokenClosureKind::NonClosure, gk);
        
        // TODO: There has to be a better way of doing this.
        let case = Expr::Class(CharClass::new(vec![
            ClassRange {begin: '"', end: '#'}, // covers ", #
            ClassRange {begin: '\\', end: '\\'}, // covers \ 
            ClassRange {begin: '0', end: '9'}, // covers digit
            // covers all letters
            ClassRange {begin: '\u{85}', end: '\u{85}'},
            ClassRange {begin: '\u{a0}', end: '\u{a0}'},
            ClassRange {begin: '\u{1680}', end: '\u{1680}'},
            ClassRange {begin: '\u{2000}', end: '\u{200a}'},
            ClassRange {begin: '\u{2028}', end: '\u{2029}'},
            ClassRange {begin: '\u{202f}', end: '\u{202f}'},
            ClassRange {begin: '\u{205f}', end: '\u{205f}'},
            ClassRange {begin: '\u{3000}', end: '\u{3000}'},
        ]));

        /* Now finally here's the actual grammar... */
        // FIXME: This won't actually work. Must rewrite with nom
        match buf {
            /* Ignore comments */
            ('#', Repeat {
                e: box AnyCharNoNL,
                r: Repeater::ZeroOrMore,
                greedy: true
            }) => continue,

            /* On ANY group-close symbol, we end the current group */
            close_pattern => {
                /* However, we have to check to make sure that the symbol matches */
                let candidate_close = group_close_under_token();
                /* The expected group close comes packed with the position of the opening symbol */
                let (group_close, group_close_at) = group_close;
                /* This is a matching close */
                if candidate_close == group_close {
                    Ok(close_group())
                }
                /* This is not a matching close */
                else {
                    match candidate_close {
                        /* No close before EOF: Failure is positioned at the opening symbol */
                        GroupCloseToken::Eof => Err(token::incomplete_at(group_close_at,
                            &format!("Did not find matching {} anywhere before end of file. Opening symbol:", group_close))),
                        /* Close present, but wrong: Failure is positioned at closing symbol */
                        GroupCloseToken::Char(_) => Err(parse_fail(
                            format!("Expected closing {} but instead found {}", group_close, candidate_close))
                        )
                    }
                }
            }
            
            /* Quoted string */
            | map!(quoted_string TokenContents::String)
            /*
            '"' => add_to_line_proceed(make_token_here(TokenContents::String(Cow::from(quoted_string()?)))),
            */
            
            /* Floating point number */
            | map!(number_pattern, TokenContents::Number)
            /*
            number_pattern => add_single(|x| 
                if x.len() < 3 {
                    TokenContents::Number(x.parse().unwrap())
                } else {
                    match &x[..2] {
                        "0b" | "0o" => TokenContents::Number(float_of_int (int_of_string u32::from_str_radix(x)),
                        "0o" => u32::from_str_radix
                        _           => TokenContents::Number(float_of_string x)
                    }
                }
            )
            */
            
            
            /* Local scope variable */
            | map!(word_pattern, |w| TokenContents::Word(Cow::from(w))),

            /* Line demarcator */
            ';' => new_line_proceed(),

            /* Line demarcator (but remember, we have to track newlines) */
            '\n' => {
                state_newline();
                new_line_proceed()
            }

            /* Reader instructions.
               TODO: A more general system for reader instructions; allow tab after \version */
            "\\version 0.1" |
            "\\version 0.2" |
            "\\version 0.3b" => { escape(true); continue } /* Ignore to end of line, don't consume */
            '\\' => { escape(false); continue }           /* Ignore to end of line and consume it */

            /* Ignore whitespace */
            white_space => continue,

            /* On groups or closures, open a new parser (NO TCO AT THIS TIME) and add its result token to the current line */
            '(' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Plain)),
            '{' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Scoped)),
            '[' => add_to_line_proceed(open_ordinary_group(TokenGroupKind::Box(BoxKind::NewObject))),
            a if a == case => add_single(TokenContents::Symbol),
            _ => Err(parse_fail("Unexpected character")) /* Probably not possible? */
		}
	}
}

// groupCloseHumanReadable
impl fmt::Display for GroupCloseToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	use self::GroupCloseToken::*;
        match *self {
        	Eof         => f.write_str("end of file"),
        	Char(ref s) => write!(f, "\"{}\"", s),
        }
    }
}

type GroupCloseRecord = (GroupCloseToken, token::CodePosition);

/* Entry point to tokenize, takes a filename and a lexbuf */
/* TODO: Somehow strip blank lines? */
fn tokenize(enclosingKind, name, buf) -> token::Token {
	/* -- Helper regexps -- */
	
}


































named!(comment, preceded!(tag!("#"), take_until!("\n")));

named!(string, delimited!(
	tag!("\""),
	
	tag!("\"")
));

named!(string<String>, delimited!(
	tag!("\""),
	map_res!(
		escaped_transform!(is_not!("\\"), '\\', alt!(
			value!(b"\\", tag!("\\"))
			| value!(b"\"", tag!("\""))
			| value!(b"n", tag!("\n"))
			//| _ -> parseFail "Unrecognized escape sequence"
		)),
		String::from_utf8
	),
	tag!("\"")
));
