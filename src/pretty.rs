/* Pretty-printers for types from various other files */

use std::cmp::Ordering;

use token::{
	CodeSequence,
	Token,
	TokenContents,
	TokenGroup,
	TokenGroupKind,
};
use options;
use value;
use value::{
	TableValue,
	Value,
	ClosureExec,
	ClosureValue,
	RegisterState,
};


/* --- Code printers --- */

/* "Disassemble" a token tree into a human-readable string (specializable) */
pub fn dump_code_tree_general(group_printer: fn(&Token, String, String, CodeSequence) -> String, token: &Token) -> String {
    match token.contents {
    /* For a simple (nongrouping) token, return a string for just the item */
		TokenContents::Word (x) |
		TokenContents::Symbol (x) => x.to_string(),
		TokenContents::String (x) => format!("\"{}\"", x),
		TokenContents::Atom (x) => format!(".{}", x),
		TokenContents::Number (x) => x.to_string(),
		
		TokenContents::Group (TokenGroup {kind, closure, items}) => {
		    let (l, r) = match kind {
		        TokenGroupKind::Plain => ("(", ")"),
		        TokenGroupKind::Scoped => ("{", "}"),
		        TokenGroupKind::Box (_) => ("[", "]"),
		    };
		    
		    let l = match closure {
		        TokenGroupKind::NonClosure =>
		        	"".to_string(),
		        TokenGroupKind::ClosureWithBinding (_, ref binding) => 
		        	format!("^{}", binding.join(" ")),
		    } + l;
		    
		    /* GroupPrinter is an argument function which takes the left group symbol, right group
		       symbol, and group contents, and decides how to format them all. */
		    group_printer(token, l, r.to_string(), items)
		}
	}
}

/* "Disassemble" a token tree into a human-readable string (specialized for looking like code) */
pub fn dump_code_tree_terse(token: &Token) -> String {
    fn group_printer(token: &Token, mut l: String, r: String, items: CodeSequence) -> String {
    	let eachline = |tokens| tokens.iter()
			.map(|t| dump_code_tree_general(group_printer, t))
			.collect::<Vec<_>>()
			.join(" ");
		
		l + &items.iter()
			.map(eachline)
			.collect::<Vec<_>>()
			.join("; ")
		+ &r
    }
    
    dump_code_tree_general(group_printer, token)
}

/* "Disassemble" a token tree into a human-readable string (specialized to show token positions) */
pub fn dump_code_tree_dense(token: &Token) -> String {
    fn one_token(x: &Token) -> String {
    	format!("{} {}", x.at, dump_code_tree_general(group_printer, x))
    }
    
    fn group_printer(token: &Token, l: String, r: String, items: CodeSequence) -> String {
    	let eachline = |tokens| tokens.iter()
			.map(one_token)
			.collect::<Vec<_>>()
			.join("\n");
		
    	l + "\n" + &items.iter()
    		.map(eachline)
    		.collect::<Vec<_>>()
    		.join("\n")
    	+ "\n" + &r
    }
    
    dump_code_tree_general(group_printer, token)
}

/* Pretty print for RegisterState. */
pub fn dump_register_state(register_state: &RegisterState) -> String {
    match *register_state {
		RegisterState::LineStart (ref v, _) =>
			format!("LineStart:{}", v),
		RegisterState::FirstValue (ref v, _) =>
			format!("FirstValue:{}", v),
		RegisterState::PairValue (ref v1, ref v2, _, _) =>
			format!("PairValue:{},{}", v1, v2),
	}
}

/* --- Value printers --- */

/* Re-escape string according to the Emily reader's rules */
pub fn escape_string(s: &str) -> String {
    let mut sb = String::with_capacity(s.len() + 2);
    sb.push('"');
    for c in s.chars() {
    	match c {
		    '"' | '\\' => {
		    	sb.push('\\');
		    	sb.push(c);
		    }
		    '\n' => sb.push_str("\\n"),
		    c => sb.push(c),
		}
    }
    sb.push('"');
    sb
}

pub fn angle_wrap(s: &str) -> String {
	format!("<{}>", s)
}

pub fn id_string_for_table(t: &TableValue) -> String {
    match t.get(value::ID_KEY) {
        None => "UNKNOWN".to_string(),
        Some (&Value::Float (v)) => (v as i32).to_string(),
        _ => "INVALID".to_string() /* Should be impossible */
    }
}

pub fn id_string_for_value(v: &Value) -> String {
	match *v {
		Value::Table (ref t) | Value::Object (ref t) =>
			id_string_for_table(t),
		_ => "UNTABLE".to_string(),
	}
}

pub fn dump_value_tree_general(wrapper: fn(&str, &Value) -> String, v: &Value) -> String {
    match *v {
        Value::Null => "<null>".to_string(),
        Value::True => "<true>".to_string(),
        Value::Float (v) => v.to_string(),
        Value::String (s) => escape_string(s),
        Value::Atom (s) => format!(".{}", s),
        Value::BuiltinFunction (_) => "<builtin>".to_string(),
        Value::BuiltinMethod (_) => "<object-builtin>".to_string(),
        Value::BuiltinUnaryMethod (_) => "<property-builtin>".to_string(),
        Value::Closure (ClosureValue {exec: e, need_args: n}) => {
            let tag = match e {
            	ClosureExec::User (_) => "closure",
            	ClosureExec::Builtin (_) => "closure-builtin"
            };
            format!("<{}/{}>", tag, n)
        }
        Value::Table (_) => wrapper("scope", v), /* From the user's perspective, a table is a scope */
        Value::Object (_) => wrapper("object", v),
        Value::Continuation (_) => "<return>".to_string()
    }
}

pub fn simple_wrapper(label: &str, obj: &Value) -> String {
	angle_wrap(label)
}

pub fn label_wrapper(label: &str, obj: &Value) -> String {
	angle_wrap(match *obj {
		Value::Table (ref t) | Value::Object (ref t) =>
			&format!("{}:{}", label, id_string_for_table(t)),
		_ => label,
	})
}

/* FIXME: The formatting here is not even a little bit generalized. */

pub fn dump_value_unwrapped_table(t: &TableValue) -> String {
	" = [\n            ".to_string()
	+ &t.iter()
		.map(|&(v1, v2)| format!("{} = {}", v1, v2))
		.collect::<Vec<_>>()
		.join("\n            ")
	+ "\n        ]"
}

pub fn dump_value_table(v: &Value) -> String {
	v.to_string() + match *v {
		Value::Table (ref t) | Value::Object (ref t) => &dump_value_unwrapped_table(t),
		_ => "",
	}
}

pub fn dump_value_new_table(v: &Value) -> String {
	if options::RUN.trace_set {
		dump_value_table(v)
	} else {
		v.to_string()
	}
}

/* Normal "print" uses this */
pub fn dump_value_for_user(v: &Value) -> String {
	match *v {
		Value::String (ref s) | Value::Atom (ref s) => s.clone(),
		_ => v.to_string(),
	}
}

/* --- repl.ml helper printers --- */

/* FIXME: Can all these various display functions be condensed at all? */
/* Also, shouldn't more of this be exposed to code? */

/* Should the REPL show a key/value pair? if not hide it. */
pub fn should_show_item(&&(k, _): &&(&Value, &Value)) -> bool {
    !vec![value::PARENT_KEY, value::HAS_KEY, value::SET_KEY, value::LET_KEY]
    	.contains(k)
}

/* Sort items in objects/tables by key name */
pub fn sort_items(&(k1, v1): &(&Value, &Value), &(k2, v2): &(&Value, &Value)) -> Ordering {
    match (*k1, *k2) {
		(Value::Atom (s1), Value::Atom (s2)) => s1.cmp(s2),
		(Value::Atom (_), _) => Ordering::Less,
		(_, Value::Atom (_)) => Ordering::Greater,
		(Value::Float (n1), Value::Float (n2)) => n1.cmp(n2),
		_ => Ordering::Equal,
	}
}

/* Display the key atom -- special-cased to avoid using a dot, since defns don't use them */
pub fn display_key(k: &Value) -> String {
    match *k {
		Value::Atom (s) => s.clone(),
		Value::Float (n) => format!("<{}>", n),
		_ => "<error>".to_string()
	}
}

/* (Optionally) truncate a string and append a suffix */
pub fn truncate(mut s: String, limit_at: usize, reduce_to: usize, suffix: &str) -> String {
    if s.len() > limit_at {
    	s.truncate(reduce_to);
    	s + suffix
    }
    else {
    	s
    }
}

/* Provide a compact view of tables/objects for the REPL */
pub fn display_table(t: &TableValue) -> String {
    let items = t.iter()
    	.cloned()
    	.collect::<Vec<_>>()
    	.sort_by(sort_items);
	
    let ordered = items.iter()
    	.filter(should_show_item);
    
    let f = |&(k, v)| display_key(k) + " = " + &repl_display(v, false);
	
    let out = match &*ordered.map(f).collect::<Vec<_>>() {
        [] => "[...]".to_string(),
        toks => format!("[{}; ...]", toks.join("; ")),
    };
    
    truncate(out, 74, 72, "...]")
}

/* Create a string representation of a value for the REPL */
pub fn repl_display(value: &Value, recurse: bool) -> String {
    match *value {
        Value::Null => "null".to_string(),
        Value::True => "true".to_string(),
        Value::Float (n) => n.to_string(),
        Value::String (ref s) => escape_string(s),
        Value::Atom (ref s) => format!(".{}", s),
        Value::Table (ref t) | Value::Object (ref t) =>
            if recurse { display_table(t) } else { "<object>".to_string() },
        _ => dump_value_for_user(value),
    }
}
