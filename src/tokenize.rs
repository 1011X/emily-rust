/* Functions to create ASTs from strings:

   This is Emily's "parser", which I'm calling a tokenizer since it doesn't do much.
   It scans left to right, categorizing tokens, pushing on left parens and popping on right.
   The parts we'd think of a "parser" as usually doing will be handled in a second,
   currently unimplemented, macro-processing step. */

use std::io;
use std::result;
use std::borrow::Cow;

use nom;
use self::regex_syntax::{
    Expr,
    Repeater,
    CharClass,
    ClassRange,
};

use macros;
use options;
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

struct TokenizeState {
    line_start: isize,
    line: isize
}

#[derive(Clone, Copy)]
enum GroupCloseToken { Eof, Char(String) }

type GroupCloseRecord = (GroupCloseToken, CodePosition);

fn group_close_human_readable(kind: &GroupCloseToken) -> Cow<'static, str> {
    match *kind {
        GroupCloseToken::Eof     => Cow::from("end of file"),
        GroupCloseToken::Char(s) => Cow::from(format!("\"{}\"", s)),
    }
}

/* Entry point to tokenize, takes a filename and a lexbuf */
/* TODO: Somehow strip blank lines? */
pub fn tokenize(enclosing_kind: TokenGroupKind, name: CodeSource, mut buf: String) -> Result<Token, Error> {
    /* -- Helper regexps -- */
    named!(octal_number, re_bytes_find!("^0o[0-7]+"));
    named!(hex_number, re_bytes_find!("^0x[:xdigit:]+"));
    named!(binary_number, re_bytes_find!("^0b[01]+"));
    named!(word_pattern, re_bytes_find!("^[:alpha:][:alnum:]*"));
    named!(float_pattern, re_bytes_find!("^(\.[:digit:]+|[:digit:]+(\.[:digit:])?([eE][-+]?[:digit:]+)?)"));
    
    named!(number_pattern<f64>, alt!(
        hex_number => { |i| {
            u32::from_str_radix(&i[2..], 16).unwrap() as f64
        }}
        | binary_number => { |i| {
            u32::from_str_radix(&i[2..], 2).unwrap() as f64
        }}
        | octal_number => { |i| {
            u32::from_str_radix(&i[2..], 8).unwrap() as f64
        }}
        | float_pattern => { |i| i.parse().unwrap() }
    ));
    
    named!(comment, re_bytes_find!("^#[^\n]*"));
    
    /* Helper function: We treat a list as a stack by appending elements to the beginning,
       but this means we have to do a reverse operation to seal the stack at the end. */
    // XXX: should never run because `Vec`s append elements at the end rather than the
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
        if unsafe { options::RUN.step_macro } { 
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
    // FIXME: keep track of newlines for error purposes
    let quoted_string = closure!(delimited!(
        char!('"'),
        map_res!(
            escaped_transform!(is_not!("\\\""), '\\', alt!(
                char!('\\')  => { |_| b"\\" }
                | char!('"') => { |_| b"\"" }
                | char!('n') => { |_| b"\n" }
                /* TODO: devour newlines */
            )),
            String::from_utf8
        ),
        char!('"')
    ));
    
    /* Sub-parser: backslash. Eat up to, and possibly including, newline */
    let escape = |seen_text| {
        let backtrack = || Sedlexing.backtrack(buf);
        loop {match buf {
            /* Have reached newline. We're done. If no command was issued, eat newline. */
            '\n' => Ok(if seen_text { backtrack() } else { state_newline() }),
            /* Skip over white space until newline is reached */
            white_space => escape(seen_text),
            /* A second backslash? Okay, back out and let the main loop handle it */
            '\\' => Ok(backtrack()),
            /* Comments are allowed after a backslash, and ignored as normal. */
            '#', Star (Compl '\n') => escape(seen_text),
            /* User probably did not intend to concatenate with blank line. */
            eof => if seen_text { Ok(backtrack()) } else {
                Err(incomplete_fail("Found EOF immediately after backslash, expected token or new line."));
            },
            /* TODO: Ignore rather than error */
            any => Err(parse_fail("Did not recognize text after backslash.")),
            _ => unreachable!()
        }
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
        let close_pattern = closure!(re_bytes_find!("^[})\]$]"));
        
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
        }

        /* Recurse with the group_seed and lines we started with, & the argument pushed onto the current line */
        /* Notice state_new_line and add_to_line_proceed are using different notions of a "line". */
        let add_to_line_proceed = |x| {line.push(x); line};

        /* Recurse with the group_seed we started with, the current line pushed onto the CodeSequence, & a new blank line */
        let new_line_proceed = || proceed_with_lines(lines_plus_line(), vec![]);

        /* Complete processing the current group by completing the current CodeSequence & feeding it to the group_seed. */
        let close_group = || group_seed(group_initializer, lines_plus_line());

        /* Helper: Given a String -> TokenContents mapping, make the token, add it to the line and recurse */
        let add_single = <F: Fn(String) -> TokenContents>|constructor| {
            add_to_line_proceed(make_token_here(constructor(matched_lexemes())))
        }
        
        /* Helper: Function-ized Symbol constructor */
        let make_symbol = TokenContents::Symbol;
        
        /* Helper: See if lines contains anything substantial */
        let any_nonblank = |mut x| loop { match *x {
            [] => return false,
            [l, ..more] if l.is_empty() => x = more.to_vec(),
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
            ClassRange {begin: 0x85 as char, end: 0x85 as char},
            ClassRange {begin: 0xa0 as char, end: 0xa0 as char},
            ClassRange {begin: 0x1680 as char, end: 0x1680 as char},
            ClassRange {begin: 0x2000 as char, end: 0x200a as char},
            ClassRange {begin: 0x2028 as char, end: 0x2029 as char},
            ClassRange {begin: 0x202f as char, end: 0x202f as char},
            ClassRange {begin: 0x205f as char, end: 0x205f as char},
            ClassRange {begin: 0x3000 as char, end: 0x3000 as char},
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
                            &format!("Did not find matching {} anywhere before end of file. Opening symbol:", group_close_human_readable(group_close)))),
                        /* Close present, but wrong: Failure is positioned at closing symbol */
                        GroupCloseToken::Char(_) => Err(parse_fail(
                            format!("Expected closing {} but instead found {}", group_close_human_readable(group_close), group_close_human_readable(candidate_close)))
                        )
                    }
                }
            }
            
            /* Quoted string */
            | quoted_string => { TokenContents::String }
            /*
            '"' => add_to_line_proceed(make_token_here(TokenContents::String(Cow::from(quoted_string()?)))),
            */
            
            
            /* Floating point number */
            | number_pattern => { TokenContents::Number }
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
            | word_pattern => { |w| TokenContents::Word(Cow::from(w)) },

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

    /* When first entering the parser, treat the entire program as implicitly being surrounded by parenthesis */
    proceed((GroupCloseToken::Eof, current_position()), token::make_group(current_position(), TokenClosureKind::NonClosure, enclosing_kind), vec![], vec![], vec![])
}

/* Tokenize entry point typed to channel */
pub fn tokenize_channel<C: io::Read>(source: CodeSource, channel: C) -> Result<Token, Error> {
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
        TokenContents::Group(g) => Ok(g.items),
        _ => Err(format!("Internal error: Object in wrong place {}", token.at))
    }
}

pub fn snippet(source: CodeSource, st: String) -> Result<CodeSequence, String> {
    match tokenize_string(source, st) {
        Ok(v) => unwrap(v),       
        Err(e) => Err(format!("Internal error: Interpreter-internal code is invalid: {}", e)),
    }
}
