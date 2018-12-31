/* Data representation for an AST. */

use std::fmt;
//use std::string::ToString;
use std::path::PathBuf;

/* Records the original source file of a token */
#[derive(Clone, Hash)]
pub enum CodeSource {
    Stdin,
    Cmdline,
    Unknown,
    Internal(&'static str),
    File(PathBuf)
}

/* Make CodeSource human-readable */
// fileNameString
impl fmt::Display for CodeSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodeSource::Stdin   => f.write_str("<input>"),
            CodeSource::Cmdline => f.write_str("<commandline>"),
            CodeSource::Unknown => f.write_str("<unknown>"),
            
            CodeSource::Internal(s) => write!(f, "<internal:{}>", s),
            CodeSource::File(s)     => write!(f, "'{}'", s.display()),
        }
    }
}

/* Records the original source position of a token */
#[derive(Clone, Hash)]
pub struct CodePosition {
    file_name: CodeSource,
    line_number: usize,
    line_offset: isize,
}

/* Make CodePosition human-readable */
// positionString
impl fmt::Display for CodePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} line {} ch {}]", self.file_name, self.line_number, self.line_offset)
    }
}

/* If the group is boxed, what is returned from it? */
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoxKind { NewObject, NewScope }

/* What are the rules for descending into this group? */
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenGroupKind {
    Plain,              /* Parenthesis */
    Scoped,             /* Create a new scope within this group */
    Box(BoxKind),       /* Create a new object */
}

/* Is this group a closure? What kind? */
#[derive(Clone, Hash)]
pub enum TokenClosureKind {
    NonClosure,                            /* Is not a function */
    ClosureWithBinding(bool, Vec<String>), /* Function with argument-- arg is return?,args */
}

/* Representation of a tokenized code blob. */
/* A CodeSequence is a list of lines. A line is a list of tokens. */
/* A Token may be a group with its own CodeSequence. */
pub type CodeSequence = Vec<Vec<Token>>;

/* Data content of a group-type token */
#[derive(Clone, Hash)]
pub struct TokenGroup {
    pub kind: TokenGroupKind,          /* Group kind */
    pub closure: TokenClosureKind,     /* Closure kind, if any */
    pub group_initializer: Vec<Token>, /* Used to create scope */
    pub items: CodeSequence,           /* Group is a list of lines, lines are a list of tokens */
}

/* Data content of a token */
#[derive(Clone)]
pub enum TokenContents {
    Word(String),   /* Alphanum */
    Symbol(String), /* Punctuation-- appears pre-macro only. */
    String(String), /* "Quoted" */
    Atom(String),   /* Ideally appears post-macro only */
    Number(f64),
    Group(TokenGroup),
}

use std::hash::{Hash, Hasher};
impl Hash for TokenContents {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::TokenContents::*;
        std::mem::discriminant(self).hash(state);
        match *self {
            Word(ref s) |
            Symbol(ref s) |
            String(ref s) |
            Atom(ref s) =>
                s.hash(state),
            
            // TODO not this
            Number(f) => f.to_bits().hash(state),
            
            Group(ref tg) => tg.hash(state),
        }
    }

    //fn hash_slice<H: Hasher>(data: &[Self], state: &mut H);
}

/* A token. Effectively, an AST node. */
#[derive(Clone, Hash)]
pub struct Token {
    pub at: CodePosition,
    pub contents: TokenContents,
}

/* Quick constructor for token */
pub fn make_token(position: CodePosition, contents: TokenContents) -> Token {
    Token {at: position, contents}
}

/* Quick constructor for token, group type */
pub fn make_group(position: CodePosition, closure: TokenClosureKind, kind: TokenGroupKind, group_initializer: Vec<Token>, items: CodeSequence) -> Token {
    make_token(position, TokenContents::Group(TokenGroup {kind, closure, group_initializer, items}))
}

pub fn clone(token: &Token, contents: &TokenContents) -> Token {
    make_token(token.at.clone(), contents.clone())
}

pub fn clone_group(token: &Token, closure: &TokenClosureKind, kind: TokenGroupKind, initializer: &[Token], items: &[Vec<Token>]) -> Token {
    make_group(token.at.clone(), closure.clone(), kind, initializer.to_vec(), items.to_vec())
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenFailureKind {
    IncompleteError,
    InvalidError,
    MacroError,
}

pub struct CompilationError(pub TokenFailureKind, pub CodePosition, pub String);

// errorString
impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CompilationError(_, ref at, ref mesg) = self;
        write!(f, "Fatal error: {} {}", mesg, at)
    }
}

pub fn incomplete_at(at: &CodePosition, mesg: &str) -> CompilationError {
    CompilationError(
        TokenFailureKind::IncompleteError,
        at.clone(),
        mesg.to_owned()
    )
}

pub fn fail_at(at: &CodePosition, mesg: &str) -> CompilationError {
    CompilationError(
        TokenFailureKind::InvalidError,
        at.clone(),
        mesg.to_owned()
    )
}

pub fn fail_token(at: &Token, mesg: &str) -> CompilationError {
    fail_at(&at.at, mesg)
}
