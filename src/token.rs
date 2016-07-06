/* Data representation for an AST. */

use std::fmt;
use std::string::ToString;

use std::borrow::Cow;
use std::path::PathBuf;

/* Records the original source file of a token */
#[derive(Clone)]
pub enum CodeSource {
	Stdin,
	Cmdline,
	Unknown,
	Internal(&'static str),
	File(PathBuf)
}

/* Make CodeSource human-readable */
impl fmt::Display for CodeSource {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			CodeSource::Stdin => f.write_str("<input>"),
			CodeSource::Cmdline => f.write_str("<commandline>"),
			CodeSource::Unknown => f.write_str("<unknown>"),
			
			CodeSource::Internal(s) => f.write_fmt(format_args!("<internal:{}>", s)),
			CodeSource::File(ref s) => f.write_fmt(format_args!("'{}'", s.display())),
		}
	}
}

/* Records the original source position of a token */
#[derive(Clone)]
pub struct CodePosition {
	file_name: CodeSource,
	line_number: usize,
	line_offset: isize,
}

/* Make CodePosition human-readable */
impl fmt::Display for CodePosition {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "[{} line {} ch {}]", self.file_name, self.line_number, self.line_offset)
	}
}

/* If the group is boxed, what is returned from it? */
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BoxKind { NewObject, NewScope }

/* What are the rules for descending into this group? */
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenGroupKind {
	Plain,               /* Parenthesis */
	Scoped,              /* Create a new scope within this group */
	Box (BoxKind),       /* Create a new object */
}

/* Is this group a closure? What kind? */
#[derive(Clone)]
pub enum TokenClosureKind {
	NonClosure,                             /* Is not a function */
	ClosureWithBinding(bool, Vec<String>), /* Function with argument-- arg is return?,args */
}

/* Representation of a tokenized code blob. */
/* A CodeSequence is a list of lines. A line is a list of tokens. */
/* A Token may be a group with its own CodeSequence. */
pub type CodeSequence<'a> = Vec<Vec<Token<'a>>>;

#[derive(Clone)]
pub struct TokenGroup {
	kind: TokenGroupKind,          /* Group kind */
	closure: TokenClosureKind,     /* Closure kind, if any */
	group_initializer: Vec<Token>, /* Used to create scope */
	items: CodeSequence,           /* Group is a list of lines, lines are a list of tokens */
}

/* Data content of a token */
#[derive(Clone)]
pub enum TokenContents<'a> {
	Word(Cow<'a, str>),   /* Alphanum */
	Symbol(Cow<'a, str>), /* Punctuation-- appears pre-macro only. */
	String(Cow<'a, str>), /* "Quoted" */
	Atom(Cow<'a, str>),   /* Ideally appears post-macro only */
	Number(f64),
	Group(TokenGroup),
}

/* A token. Effectively, an AST node. */
#[derive(Clone)]
pub struct Token<'a> {
	at: CodePosition,
	contents: TokenContents<'a>,
}

/* Quick constructor for token */
pub fn make_token(position: &CodePosition, contents: &TokenContents) -> Token {
	Token {
		at: position.clone(),
		contents: contents.clone(),
	}
}

/* Quick constructor for token, group type */
pub fn make_group(position: &CodePosition, closure: &TokenClosureKind, kind: TokenGroupKind, group_initializer: &Vec<Token>, items: &CodeSequence) -> Token {
    make_token(position, &TokenContents::Group (TokenGroup {
    	kind: kind,
    	closure: closure.clone(),
    	group_initializer: group_initializer.clone(),
    	items: items.clone(),
    }))
}

pub fn clone(token: &Token, contents: &TokenContents) -> Token {
	make_token(&token.at, contents)
}

pub fn clone_group(token: &Token, closure: &TokenClosureKind, kind: &TokenGroupKind, initializer: &Vec<Token>, items: &CodeSequence) -> Token {
	make_group(&token.at, closure, kind, initializer, items)
}

/*
impl Token {
	// Quick constructor for Token
	// XXX: make_token
	pub fn new(position: &CodePosition, contents: &TokenContents) -> Token {
		Token {
			at: position.clone(),
			contents: contents.clone()
		}
	}
	
	// Quick constructor for Token, group type
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
*/

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenFailureKind {
	IncompleteError,
	InvalidError,
	MacroError
}

pub struct CompilationError(TokenFailureKind, CodePosition, String);

impl fmt::Display for CompilationError {
	
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let CompilationError(_, ref at, ref mesg) = *self;
		
		f.write_fmt(format_args!("Fatal error: {} {}", mesg, at))
	}
}

pub fn incomplete_at(at: &CodePosition, mesg: &str) -> Result<(), CompilationError> {
	Err(CompilationError(TokenFailureKind::IncompleteError, at.clone(), mesg.to_owned()))
}

pub fn fail_at(at: &CodePosition, mesg: &str) -> Result<(), CompilationError> {
	Err(CompilationError(TokenFailureKind::InvalidError, at.clone(), mesg.to_owned()))
}

pub fn fail_token(at: &Token, mesg: &str) -> Result<(), CompilationError> {
	fail_at(&at.at, mesg)
}
