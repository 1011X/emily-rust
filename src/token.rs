/* Data representation for an AST. */

use CodeSource::*;
use TokenFailureKind::*;

/* Records the original source file of a token */
#[derive(Clone)]
pub enum CodeSource {
	Stdin,
	Cmdline,
	Unknown,
	Internal(String),
	File(String)
}

/* Records the original source position of a token */
#[derive(Clone)]
pub struct CodePosition {
	file_name : CodeSource,
	line_number : isize,
	line_offset : isize
}

/* Make CodePosition.file_name human-readable */
pub fn file_name_string(n: &CodeSource) -> String {
	match *n {
		Stdin => "<input>".to_string(),
		Cmdline => "<commandline>".to_string(),
		Unknown => "<unknown>".to_string(),
		Internal(ref s) => format!("<internal:{}>", s),
		File(ref s) => format!("'{}'", s)
	}
}

/* Make CodePosition human-readable */
pub fn position_string(p: &CodePosition) -> String {
	format!("[{} line {} ch {}]", file_name_string(&p.file_name), p.line_number, p.line_offset)
}

/* If the group is boxed, what is returned from it? */
#[derive(Clone)]
pub enum BoxKind {
	NewObject,
	NewScope
}

/* What are the rules for descending into this group? */
#[derive(Clone)]
pub enum TokenGroupKind {
	Plain,               /* Parenthesis */
	Scoped,              /* Create a new scope within this group */
	Box(BoxKind)         /* Create a new object */
}

/* Is this group a closure? What kind? */
#[derive(Clone)]
pub enum TokenClosureKind {
	NonClosure,                              /* Is not a function */
	ClosureWithBinding(Vec<(bool, String)>)  /* Function with argument-- arg is return?,args */
}

/* Representation of a tokenized code blob. */
/* A CodeSequence is a list of lines. A line is a list of tokens. */
/* A Token may be a group with its own CodeSequence. */
pub type CodeSequence = Vec<Vec<Token>>;

/* Data content of a token */
#[derive(Clone)]
pub enum TokenContents {
	Word(String),   /* Alphanum */
	Symbol(String), /* Punctuation-- appears pre-macro only. */
	String(String), /* "Quoted" */
	Atom(String),   /* Ideally appears post-macro only */
	Number(f64),
	
	/* Data content of a group-type token */
	// XXX: TokenGroup
	Group {
		kind : TokenGroupKind,       /* Group kind */
		closure : TokenClosureKind,  /* Closure kind, if any */
		items : CodeSequence         /* Group is a list of lines, lines are a list of tokens */
	}
}

/* A token. Effectively, an AST node. */
#[derive(Clone)]
pub struct Token {
	at : CodePosition,
	contents : TokenContents
}

impl Token {
	/* Quick constructor for Token */
	// XXX: make_token
	pub fn new(position: &CodePosition, contents: &TokenContents) -> Token {
		Token {
			at : position.clone(),
			contents : contents.clone()
		}
	}
	
	/* Quick constructor for Token, group type */
	// XXX: make_group
	pub fn from_group(position: &CodePosition, closure: &TokenClosureKind, kind: &TokenGroupKind, items: &CodeSequence) -> Token {
		Token {
			at: position.clone(),
			contents: TokenContents::Group {
				kind: kind.clone(),
				closure: closure.clone(),
				items: items.clone() 
			}
		}
	}
	
	pub fn clone(&self, contents: &TokenContents) -> Token {
		Self::new(&self.at, contents)
	}
	
	pub fn clone_group(&self, closure: &TokenClosureKind, kind: &TokenGroupKind, items: &CodeSequence) -> Token {
		Self::from_group(&self.at, closure, kind, items)
	}
}

pub enum TokenFailureKind {
	IncompleteError,
	InvalidError,
	MacroError
}

pub struct CompilationError(TokenFailureKind, CodePosition, String);

pub fn incomplete_at(at: &CodePosition, mesg: String) -> Result<(), CompilationError> {
	Err(CompilationError(IncompleteError, at.clone(), mesg))
}

pub fn fail_at(at: &CodePosition, mesg: String) -> Result<(), CompilationError> {
	Err(CompilationError(InvalidError, at.clone(), mesg))
}

pub fn fail_token(at: &Token, mesg: String) -> Result<(), CompilationError> {
	fail_at(&at.at, mesg)
}

pub fn error_string(CompilationError(_, at, mesg): CompilationError) -> String {
	format!("Fatal error: {} {}", mesg, position_string(&at))
}
