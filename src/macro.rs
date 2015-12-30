/* Macro processing */
#![feature(box_syntax)]

#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::sync::{Once, ONCE_INIT};

use token;

use token::{
    TokenContents,
    TokenClosureKind,
    TokenGroupKind
};

pub fn fail_at(at: &CodePosition, mesg: String) -> Result<(), token::CompilationError> {
    Err (token::CompilationError (Token.MacroError, at, mesg))
}
pub fn fail_token(at: &Token, mesg: String) -> Result<(), token::CompilationError> {
    fail_at(&at.at, mesg)
}

/* Last thing we always do is make sure no symbols survive after macro processing. */
pub fn verify_symbols(l: SingleLine) -> SingleLine {
    for token in l.iter() {
        if let &Token {contents: TokenContents::Symbol(ref s), ref at} = token {
            token::fail_at(at, format!("Unrecognized symbol {}", s));
        }
    }
    l
}

/* Types for macro processing. */

enum MacroPriority { L (f64), R (f64) } /* See builtinMacros comment */

type SingleLine = Vec<Token>;

/* Note what a single macro does:
    The macro processor sweeps over a line, keeping a persistent state consisting of
    "past" (tokens behind cursor, reverse order) "present" (token at cursor) and
    "future" (tokens ahead of cursor). A macro replaces all 3 with a new line. */
type MacroFunction = Box<Fn(SingleLine, Token, SingleLine) -> SingleLine>;

struct MacroSpec {
    priority: MacroPriority,
    spec_function: MacroFunction,
}

struct MacroMatch {
    matchFunction: MacroFunction,
    past: SingleLine,
    present: token::Token,
    future: SingleLine
}

/* The set of loaded macros lives here. */
lazy_static! {
	pub static ref mut MACRO_TABLE: HashMap<String, MacroSpec> = HashMap::with_capacity(1);
}

/* All manufactured tokens should be made through clone, so that position information is retained */
pub fn clone_atom(at: &CodePosition, s: String) -> Token {
    token::clone(at, TokenContents::Atom (s))
}

pub fn clone_word(at: &CodePosition, s: String) -> Token {
    token::clone(at, TokenContents::Word (s))
}

pub fn clone_group(at: &CodePosition) -> Token {
    token::clone_group(at, TokenClosureKind::NonClosure, TokenGroupKind::Plain, vec![])
}

/* Note: makes no-return closures */
pub fn clone_closure(at: &CodePosition) -> Token {
    token::clone_group(at, TokenClosureKind::ClosureWithBinding (false, vec![]), TokenGroupKind::Plain, vec![])
}

/* Debug method gets to use this. */
lazy_static! {
	pub static ref NULL_TOKEN: Token = token::make_token(
		CodePosition {
		    file_name: CodeSource::Unknown,
		    line_number: 0,
		    line_offset: 0
		},
		TokenContents::Symbol ("_".to_string())
	);
}

/* Macro processing, based on whatever builtinMacros contains */
pub fn process(l: SingleLine) -> SingleLine {
    if options::RUN.step_macro {
    	println!(pretty::dump_code_tree_terse(clone_group(NULL_TOKEN, vec![l])));
    }

    /* Search for macro to process next. Priority None/line None means none known yet. */
    fn find_ideal(best_priority: Option<MacroPriority>, best_line, past: SingleLine, present, future) -> Option<MacroMatch> {
        /* Iterate cursor */
        let proceed = |priority, line| match &*future {
            /* Future is empty, so future has iterated to end of line */
            [] => line,

            /* Future is nonempty, so move the cursor forward. */
            [next_future.., next_token] => find_ideal(priority, line, past.into_iter().chain(vec![present]).collect(), next_token, next_future),
        };

        /* Iterate cursor leaving priority and line unchanged */
        let skip = || proceed(best_priority, best_line);

        /* Investigate token under cursor */
        match present.contents {
            /* Words or symbols can currently be triggers for macros. */
            TokenContents::Word (s) | TokenContents::Symbol (s) =>
                /* Is the current word/symbol a thing in the macro table? */
                let v = MACRO_TABLE.get(s).cloned();
                match v {
                    /* It's in the table; now to decide if it's an ideal match. */
                    Some (MacroSpec {priority, spec_function}) => {
                        /* True if this macro is better fit than the current candidate. */
                        let better = match (best_priority, priority) {
                            /* No matches yet, automatic win. */
                            (None, _) => true,

                            /* If associativity varies, we can determine winner based on that alone: */
                            /* Prefer higher priority, but break ties toward left-first macros over right-first ones. */
                            (Some L(left), R(right)) => left < right,
                            (Some R(left), L(right)) => left <= right,

                            /* "Process leftmost first": Prefer higher priority, break ties to the left. */
                            (Some L(left), L(right)) => left < right,

                            /* "Process rightmost first": Prefer higher priority, break ties to the right. */
                            (Some R(left), R(right)) => left <= right,
                        };
                        
                        if better {
                            proceed(Some (priority), Some (MacroMatch {
                            	past: past,
                            	present: present,
                            	future: future,
                            	match_function: spec_function
                            }));
                        }
                        /* It's a worse match than the current best guess. */
                        else {
                        	skip();
                    	}
                    }
                    /* It's not in the table. */
                    _ => skip(),
                }
            /* It's not even a candidate to trigger a macro. */
            _ => skip(),
        }
    }

    /* Actually process macro */
    match &*l {
        /* Special case: Line is empty, do nothing. */
        l @ [] => l.to_vec(),

        /* Split out first item to use as the find_ideal "present" cursor. */
        [future.., present] =>
            /* Repeatedly run find_ideal until there are no more macros in the line. */
            match find_ideal(None, None, vec![], present, future.to_vec()) {
                /* No macros triggered! Sanitize the line and return it. */
                None => verify_symbols(l),

                /* A macro was found. Run the macro, then re-process the adjusted line. */
                Some (MacroMatch {match_function, past, present, future}) => {
                    if options::RUN.step_macro {
                    	println!("    ...becomes:");
                    }
                    process(match_function(past, present, future))
                }
            },
	}
}

/* The macros themselves */

/* Support functions for macros */

pub fn new_future(at: &CodePosition, f: SingleLine) -> Token {
	clone_group(at, vec![process(f)]) /* Insert a forward-time group */
}

pub fn new_past(at: &CodePosition, p: SingleLine) -> Token {
	new_future(at, p.into_iter().rev().collect())   /* Insert a reverse-time group */
}

pub fn new_future_closure(at: &CodePosition, f: SingleLine) -> Token {
	clone_closure(at, vec![process(f)])      /* Insert a forward-time closure */
}

pub fn new_past_closure(at: &CodePosition, p: SingleLine) -> Token {
	new_future_closure(at, p.into_iter().rev().collect())  /* Insert a reverse-time closure */
}

/* A recurring pattern in the current macros is to insert a new single token
   into "the middle" of an established past and future */
/* FIXME: Inferring position from "present" will work  */
pub fn arrange_token(at: &CodePosition, past: SingleLine, present: Token, future: SingleLine) -> SingleLine {
    vec![
		new_future(at, vec![
			past.into_iter().rev().collect(),
			vec![present],
			future
		].flat_map(|l| l.into_iter()).collect())
	]
}

pub fn arrange(at: &CodePosition, past: SingleLine, present: Token, future: SingleLine) -> SingleLine {
    arrange_token(at, past, new_future(at, present), future)
}

/* Constructors that return working macros */

/* Given argument "op", make a macro to turn `a b … OP d e …` into `(a b …) .op (d e …)` */
pub fn make_splitter(atom_string: String) -> MacroFunction {
	box move |past, at, future|
		vec![new_past(at, past), clone_atom(at, atom_string), new_future(at, future)]
)

/* Given argument "op", make a macro to turn `OP a` into `((a) .op)` */
pub fn make_unary(atom_string: String) -> MacroFunction {
	box move |past, at, future| match &*future {
        [a, far_future..] =>
            arrange(at, past, vec![a, clone_atom(at, atom_string)], far_future.to_vec()),
        _ => token::fail_token(at, format!("{} must be followed by something", pretty::dump_code_tree_terse(at))),
    }
)

/* Given argument "op", make a macro to turn `OP a` into `(op (a))` */
pub fn make_prefix_unary(word_string: String) -> MacroFunction {
	box move |past, at, future| match &*future {
        [a, far_future..] =>
            arrange(at, past, vec![clone_word(at, word_string), a], far_future.to_vec()),
        _ => token::fail_token(at, format!("{} must be followed by something", pretty::dump_code_tree_terse(at))),
    }
)

/* Given argument "op", make a macro to turn `a b … OP d e …` into `(op ^(a b …) ^(d e …)` */
pub fn make_short_circuit(word_string: String) -> MacroFunction {
	box move |past, at, future|
		vec![clone_word(at, word_string), new_past_closure(at, past), new_future_closure(at, future)]
)

pub fn make_splitter_invert(atom_string: String) -> MacroFunction {
	box move |past, at, future|
    vec![clone_word(at, "not".to_string()), new_future(at,
        vec![new_past(at, past), clone_atom(at, atom_string), new_future(at, future)]
    )]
)

/* One-off macros */

/* Ridiculous thing that is only for testing the macro system itself. */
/* Prints what's happening, then deletes itself. */
pub fn debug_op(past: SingleLine, present: token::Token, future: SingleLine) -> SingleLine {
    println!("Debug macro:");
    println!("\tPast:    {}",
    	pretty::dump_code_tree_terse(
    		clone_group(
    			NULL_TOKEN,
    			past.into_iter().rev().collect()
    )));
    println!("\tPresent: {}", pretty::dump_code_tree_terse(present));
    println!("\tFuture:  {}", pretty::dump_code_tree_terse(clone_group(NULL_TOKEN, vec![future])));
    
    vec![past.into_iter().rev().collect(), future].into_iter()
    	.flat_map(|l| l.into_iter())
    	.collect()
}

/* Apply operator-- Works like ocaml @@ or haskell $ */
pub fn apply_right(past: SingleLine, at: &CodePosition, future: SingleLine) -> SingleLine {
    vec![new_past(
    	at,
    	past.into_iter().chain(vec![new_future(at, future)]).collect()
	)]
}

/* "Apply pair"; works like unlambda backtick */
pub fn backtick(past: SingleLine, at: &CodePosition, future: SingleLine) -> SingleLine {
    match &*future {
        [a, b, far_future..] =>
            arrange(at, past, vec![a, b], far_future.to_vec()),
        _ => token::fail_token(at, "` must be followed by two symbols".to_string()),
    }
}

/* Works like ocaml @@ or haskell $ */
pub fn question(past: SingleLine, at: &CodePosition, future: SingleLine) -> SingleLine {
    let result = |colon_at, cond, a, b| vec![
    	clone_word(at, "tern".to_string()),
        new_future(at, cond),
        new_future_closure(at, a),
        new_future_closure(colon_at, b)
    ];
    fn scan(a, rest) -> SingleLine {
        match &*rest {
            [more_rest.., colon_at @ Token {contents: TokenContents::Symbol (sym)}] if sym == ":" =>
                result(colon_at, past.into_iter().rev().collect(), a.into_iter().rev().collect(), more_rest.to_vec()),
                
            [more_rest.., Token {contents: TokenContents::Symbol (sym)}] if sym == "?" =>
                token::fail_token(at, "Nesting like ? ? : : is not allowed.".to_string())
            
            [more_rest.., token] =>
                scan(a.into_iter().chain(vec![token]).collect(), more_rest.to_vec()),
            
            [] => token::fail_token(at, ": expected somewhere to right of ?".to_string()),
        }
    }
    scan(vec![], future)
}

/* Assignment operator-- semantics are relatively complex. See manual.md. */
pub fn assignment(past: SingleLine, at: &CodePosition, future: SingleLine) -> SingleLine {
    /* The final parsed assignment will consist of a list of normal assignments
       and a list of ^ variables for a function. Perform that assignment here: */
    let result = |lookups, bindings| {
        /* The token to be eventually assigned is easy to compute early, so do that. */
        let rightside = match bindings {
            /* No bindings; this is a normal assignment. */
            None => new_future(at, future),

            /* Bindings exist: This is a function definition. */
            Some (bindings) => token::clone_group(at, TokenClosureKind::ClosureWithBinding (true, bindings.into_iter().rev().collect()),
                TokenScopeKind::Plain, vec![], vec![process(future)]),
        };

        /* Recurse to try again with a different command. */
        /* TODO: This is all wrong... set should be default, let should be the extension.
           However this will require... something to allow [] to work right. */
        fn result_for_command(cmd_at: CodePosition, cmd: String, lookups: SingleLine) -> SingleLine {

            /* Done with bindings now, just have to figure out what we're assigning to */
            match (&*lookups, &*cmd) {
                /* ...Nothing? */
                ([], _) => token::fail_token(at, "Found a =, but nothing to assign to.".to_string()),

                /* Sorta awkward, detect the "nonlocal" prefix and swap out let. This should be generalized. */
                ([more_lookups.., cmd_token @ Token {contents: TokenContents::Word (word), ..}], "let") if word == "nonlocal" =>
                	result_for_command(cmd_token, value::SET_KEY_STRING, more_lookups),

                /* Looks like a = b */
                ([token], _) => vec![
                	clone_word(&cmd_at, cmd),
                	/* FIXME: Nothing now prevents assigning to a number in a plain scope? */
                    match token {
                        Token {contents: TokenContents::Word (name)} =>
                        	clone_atom(token, name),
                        token => token,
                    },
                    rightside
                ],

                /* Looks like a b ... = c */
                ([more_lookups.., first_token], _) => match &*more_lookups {
                    /* Note what's happening here: We're slicing off the FINAL element, first in the reversed list. */
                    [middle_lookups.., final_token] => vec![
                    	vec![first_token],
                    	middle_lookups.into_iter().rev().collect(),
                    	vec![clone_atom(cmd_at, cmd), final_token, rightside]
                	].flat_map(|t| t.into_iter()).collect(),

                    /* Excluded by [{Token.word}] case above */
                    _ => token::fail_token(at, "Internal failure: Reached impossible place".to_string()),
                },
            }
        }

        result_for_command(at, "let".to_string(), lookups)
    };

    /* Parsing loop, build the lookups and bindings list */
    fn process_left(remaining_left: SingleLine, lookups: SingleLine, bindings: Option<Vec<String>>) -> SingleLine {
        match (remaining_left, bindings) {
            /* If we see a ^, switch to loading bindings */
            ([Token {contents: TokenContents::Symbol (sym), ..}, more_left..], None)
            if sym == "^" =>
                process_left(more_left, lookups, Some (vec![])),

            /* If we're already loading bindings, just skip it */
            ([Token {contents: TokenContents::Symbol (sym), ..}, more_left..], _)
            if sym == "^" =>
                process_left(more_left, lookups, bindings),

            /* Sanitize any symbols that aren't cleared for the left side of an = */
            ([Token {contents: TokenContents::Symbol (x), ..}, ..], _) => failwith(format!("Unexpected symbol {} to left of =", x)),

            /* We're adding bindings */
            ([Token {contents: TokenContents::Word (b), ..}, rest_past..], Some (bindings)) =>
                process_left(rest_past, lookups, Some (bindings.into_iter().chain(vec![b]).collect())),

            /* We're adding lookups */
            ([l, rest_past..], None) =>
                process_left(rest_past, lookups.into_iter().chain(vec![l]).collect(), None),

            /* There is no more past, Jump to result. */
            ([], _) => result(lookups, bindings),

            /* Apparently did something like 3 = */
            ([token, ..], _) => token::fail_token(token, format!("Don't know what to do with {} to left of =", pretty::dump_code_tree_terse(token))),
        }
    }

    process_left(past.into_iter().rev().collect(), vec![], None)
}

/* Constructor for closure constructor, depending on whether return wanted. See manual.md */
pub fn closure_construct(with_return: bool) -> MacroFunction {
    box move |past, at, future| {
        /* Scan line picking up bindings until group reached. */
        fn open_closure(bindings: Vec<String>, future: SingleLine) -> SingleLine {
            match &*future {
                /* If redundant ^s seen, skip them. */
                [Token {contents: TokenContents::Symbol (sym), ..}, more_future..]
                if sym == "^" =>
                    open_closure(bindings, more_future.to_vec()),

                /* This is a binding, add to list. */
                [Token {contents: TokenContents::Word (b), ..}, more_future..] =>
                    open_closure(bindings.into_iter().chain(vec![b]).collect(), more_future),

                /* This is a group, we are done now. */
                [Token {contents: TokenContents::Group(TokenGroup  {closure: TokenClosureKind::NonClosure, kind, group_initializer, items}}, more_future..] => match kind {
                        /* They asked for ^[], unsupported. */
                        TokenGroupKind::Box (_) => token::fail_token(at, "Can't use object literal with ^"),
                        /* Supported group */
                        _ => arrange_token(at, past, token::clone_group(at, TokenClosureKind::ClosureWithBinding (with_return, bindings.into_iter().rev().collect())), kind, group_initializer, items), more_future.to_vec()),
                },

                /* Reached end of line */
                [] => token::fail_token(at, "Body missing for closure"),

                /* Any other symbol */
                _ => token::fail_token(at, "Unexpected symbol after ^"),
            }
        }

        open_closure(vec![], future)
    }
}

/* Commas in statement */
pub fn comma(past: SingleLine, at: CodePosition, future: SingleLine) -> SingleLine {
    /* Split statement into comma-delimited sections. Generates a reverse list of token reverse-lists */
    fn gather(accumulate_line: SingleLine, accumulate_all: Vec<SingleLine>, future: SingleLine) -> Vec<SingleLine> {
        /* A new value for accumulateLine */
        let push_line = || accumulate_all.into_iter().chain(vec![accumulate_line]).collect::<Vec<_>>();
        match &*future {
            /* Finished */
            [] => push_line(),
            
            /* Another comma was found, open a new section */
            [Token {contents: TokenContents::Symbol (sym), ..}, more_future..]
            if sym == "," =>
            	gather(vec![], push_line(), more_future.to_vec()),
        	
            /* Add token to current section */
            [t, more_future..] => gather(accumulate_line.into_iter().chain(vec![t]).collect(), accumulate_all, more_future.to_vec()),
        }
    }
    /* Given reverse list of reverse-list-of-tokens, create a list of this.append statements */
    fn emit(accumulate_final: SingleLine, sub_lines: Vec<SingleLine>) -> SingleLine{
        match &*sub_lines {
            /* Finished */
            [] => accumulate_final,

            /* Blank line-- ignore it */
            [[], more_lines..] => emit(accumulate_final, more_lines.to_vec()),
            
            /* Nonempty line-- wrap TOKENS into this.append(TOKENS); */
            [tokens @ [first_token, more_tokens..], more_lines] => emit(
                accumulate_final.into_iter().chain(vec![vec![
                	clone_word(first_token, "this".to_string()),
                	clone_atom(first_token, "append".to_string()),
                	clone_group(first_token, vec![process(tokens.into_iter().rev().collect())])
            	]]).collect(),
            	more_lines.to_vec()
        	),
    	}
	}
    /* Pull apart past with gather, stitch back together with emit, we now have a list of statements we can turn into a group. */
    vec![clone_group(at, emit(vec![], gather(vec![], vec![past], future))]
}

/* Atom */
pub fn atom(past: SingleLine, at: CodePosition, future: SingleLine) -> SingleLine {
    match &*future {
        /* Look at next token and nothing else. */
        [Token {contents: TokenContents::Word (a), ..}, more_future..] =>
            arrange_token(at, past, clone_atom(at, a), more_future.to_vec()),
        _ => token::fail_token(at, "Expected identifier after .".to_string()),
    }
}

/* Splitter which performs an unrelated unary operation if nothing to the left. */
pub fn make_dual_mode_splitter(unary_atom, binary_atom) -> MacroFunction {
	box move |past, at, future| {
		let prefix_unary = make_unary(unary_atom);
		let splitter = make_splitter(binary_atom);
		match past {
		    [] | [Token {contents: TokenContents::Symbol (s), ..}, ..]
		    /* Because this is intended for unary -, special-case arithmetic. */
		    /* I don't much like this solution? */
		    if s == "*" || s == "/" || s == "%" || s == "-" || s == "+" =>
		        prefix_unary(past, at, future),
		    _ => splitter(past, at, future),
	    }
    }
}

/* Just to be as explicit as possible:

   Each macro has a priority number and a direction preference.
   If the priority number is high, the macro is high priority and it gets interpreted first.
   If sweep direction is L, macros are evaluated "leftmost first"  (moving left to right)
   If sweep direction is R, macros are evaluated "rightmost first" (moving right to left)
   If there are multiple macros of the same priority, all the L macros (prefer-left)
   are interpreted first, and all of the R macros (prefer-right) after.
   (I recommend against mixing L and R macros on a single priority.)

   Notice how priority and sweep direction differ from "precedence" and "associativity".
   They're essentially opposites. The later a splitter macro evaluates, the higher
   "precedence" the associated operator will appear to have, because splitter macros
   wrap parenthesis around *everything else*, not around themselves.
   For similar reasons, right-preference likes like left-associativity and vice versa.

   So this table goes:
    - From lowest priority to highest priority.
    - From lowest-magnitude priority number to highest-magnitude priority number.
    - From last-evaluated to earliest-evaluated macro.
    - From closest-binding operators to loosest-binding operators
      (In C language: From "high precedence" to "low precedence" operators)
*/

lazy_static! {
	pub static ref BUILTIN_MACROS = vec![
		/* Weird grouping */

		(R(20.), "`", backtick),

		/* More boolean */
		(R(30.), "!", make_prefix_unary("not")),

		/* Math */
		(R(30.), "~", make_unary("negate")),
		(R(40.), "/", make_splitter("divide")),
		(R(40.), "*", make_splitter("times")),
		(R(40.), "%", make_splitter("mod")),
		(R(50.), "-", make_dual_mode_splitter "negate" "minus")),
		(R(50.), "+", make_splitter("plus")),

		/* Comparators */
		(R(60.), "<", make_splitter("lt")),
		(R(60.), "<=", make_splitter("lte")),
		(R(60.), ">", make_splitter("gt")),
		(R(60.), ">=", make_splitter("gte")),

		(R(65.), "==", make_splitter("eq")),
		(R(65.), "!=", make_splitter_invert("eq")),

		/* Boolean */
		(R(70.), "&&", make_short_circuit("and")),
		(R(75.), "||", make_short_circuit("or")),
		(R(77.), "%%", make_short_circuit("xor")),

		/* Grouping */ /* FIXME: Would these make more sense after assignment? */
		(L(90.), ":", apply_right),
		(L(90.), "?", question),

		/* Core */
		(L(100.), "^",  closure_construct(false)),
		(L(100.), "^@", closure_construct(true)),
		(L(105.), "=",  assignment),
		(L(110.), ".",  atom),

		/* Pseudo-statement */
		(L(150.), ",", comma),
	];
}

/* Populate macro table from BUILTIN_MACROS. */
static START: Once = ONCE_INIT;
START.call_once(|| {
    for (priority, key, spec_function) in BUILTIN_MACROS {
    	MACRO_TABLE.insert(key, MacroSpec {priority, spec_function});
    }
});
