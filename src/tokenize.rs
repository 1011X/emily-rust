/* Functions to create ASTs from strings:

   This is Emily's "parser", which I'm calling a tokenizer since it doesn't do much.
   It scans left to right, categorizing tokens, pushing on left parens and popping on right.
   The parts we'd think of a "parser" as usually doing will be handled in a second,
   currently unimplemented, macro-processing step. */

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
