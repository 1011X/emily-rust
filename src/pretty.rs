/* Pretty-printers for types from various other files */

mod token;
mod options;
mod value;

use std::cmp::Ordering;

use token::{
	TableValue,
	TokenContents,
	TokenGroupKind
}

use value::{
	Value,
	ClosureExec
}

/* --- Code printers --- */

/* "Disassemble" a token tree into a human-readable string (specializable) */
fn dump_code_tree_general<F>(group_printer: F, token: &Token) -> String
	where F: Fn(&Token, String, String, CodeSequence) -> String {
    match token.contents {
    /* For a simple (nongrouping) token, return a string for just the item */
		TokenContents::Word(x) | TokenContents::Symbol(x) => x,
		TokenContents::String(x) => format!("\"{}\"", x),
		TokenContents::Atom(x) => format!(".{}", x),
		TokenContents::Number(x) => x.to_string(),
		TokenContents::Group {kind: kind, closure: closure, items: items} => {
		    let (mut l, r) = match kind {
		        TokenGroupKind::Plain => ("(".to_string(), ")".to_string()),
		        TokenGroupKind::Scoped => ("{".to_string(), "}".to_string()),
		        TokenGroupKind::Box(..) => ("[".to_string(), "]".to_string())
		    };
		    l = match closure {
		        TokenGroupKind::NonClosure => "".to_string(),
		        TokenGroupKind::ClosureWithBinding (_,binding) => 
		        	format!("^{}", if binding == [] { "" } else { binding.connect(" ") })
		    } + &l;
		    /* GroupPrinter is an argument function which takes the left group symbol, right group
		       symbol, and group contents, and decides how to format them all. */
		    group_printer(token, l, r, items)
		}
	}
}

/* "Disassemble" a token tree into a human-readable string (specialized for looking like code) */
fn dump_code_tree_terse(token: &Token) -> String {
    fn group_printer(token: &Token, l: String, r: String, items: CodeSequence) {
    	fn eachline(tokens: &Vec<Token>) -> String {
    		tokens.iter()
    			.map(|t| dump_code_tree_general(group_printer, t))
    			.collect::<Vec<_>>()
    			.connect(" ")
    	}
		l + &items.iter()
			.map(eachline)
			.collect::<Vec<_>>()
			.connect("; ")
		+ &r
    }
    dump_code_tree_general(group_printer, token)
}

/* "Disassemble" a token tree into a human-readable string (specialized to show token positions) */
fn dump_code_tree_dense(token: &Token) -> String {
    fn one_token(x: &Token) -> String {
    	format!("{} {}", token::position_string(&x.at), dump_code_tree_general(group_printer, x))
    }
    fn group_printer(token: &Token, l: String, r: String, items: CodeSequence) -> String {
    	fn eachline(tokens: &Vec<Token>) -> String {
    		tokens.iter()
    			.map(one_token)
    			.collect::<Vec<_>>()
    			.connect("\n")
    	}
    	l + "\n" + &items.iter()
    		.map(eachline)
    		.collect::<Vec<_>>()
    		.connect("\n")
    	+ "\n" + &r
    }
    dump_code_tree_general(group_printer, token)
}

/* --- Value printers --- */

/* Re-escape string according to the Emily reader's rules */
fn escape_string(s: String) -> String {
    let mut sb = String::with_capacity(s.len() + 2);
    sb.push('"');
    for c in s.chars() {
    	match c {
		    '"' | '\\' => {
		    	sb.push('\\');
		    	sb.push(c);
		    },
		    '\n' => sb.push_str("\\n"),
		    _ => sb.push(c)
		}
    }
    sb.push('"');
    sb
}

fn angle_wrap(s: String) -> String {
	format!("<{}>", s)
}

fn id_string_for_table(t: &TableValue) -> String {
    match t.get(value::ID_KEY) {
        None => "UNKNOWN".to_string(),
        Some(&Value::FloatValue(v)) => (v as isize).to_string(),
        _ => "INVALID".to_string() /* Should be impossible */
    }
}

fn id_string_for_value(v: &Value) -> String {
	match *v {
		Value::TableValue(ref t) | Value::ObjectValue(ref t) => id_string_for_table(t),
		_ => "UNTABLE".to_string()
	}
}

fn dump_value_tree_general<F>(wrapper: F, v: &Value) -> String
	where F: Fn(String, &Value) -> String {
    match *v {
        Value::Null => "<null>".to_string(),
        Value::True => "<true>".to_string(),
        Value::FloatValue (v) => v.to_string(),
        Value::StringValue (s) => escape_string(s),
        Value::AtomValue (s) => format!(".{}", s),
        Value::BuiltinFunctionValue (_) => "<builtin>".to_string(),
        Value::BuiltinMethodValue (_) => "<object-builtin>".to_string(),
        Value::BuiltinUnaryMethodValue (_) => "<property-builtin>".to_string(),
        Value::ClosureValue {exec:e, needArgs:n} => {
            let tag = match e {
            	ClosureExec::ClosureExecUser (_) => "closure",
            	ClosureExec::ClosureExecBuiltin (_) => "closure-builtin"
            };
            format!("<{}/{}>", tag, n)
        },
        Value::TableValue     (_) => wrapper("scope".to_string(), v), /* From the user's perspective, a table is a scope */
        Value::ObjectValue    (_) => wrapper("object".to_string(), v),
        Value::ContinuationValue (_) => "<return>".to_string()
    }
}

fn simple_wrapper(label: String, obj: &Value) -> String {
	angle_wrap(label)
}

fn label_wrapper(label: String, obj: &Value) -> String {
	angle_wrap(match *obj {
		Value::TableValue (t) | Value::ObjectValue (t) => label + ":" + &id_string_for_table(t),
		_ => label
	})
}

fn dump_value(v: &Value) -> String {
    let wrapper = if options::run.track_objects { label_wrapper } else { simple_wrapper };
    dump_value_tree_general(wrapper, v)
}

/* FIXME: The formatting here is not even a little bit generalized. */
fn dump_value_table(v: &Value) -> String {
    dump_value(v) + match *v {
        Value::TableValue (t) | Value::ObjectValue (t) => " = [\n            ".to_string() + 
			&t.iter()
				.map(|(v1, v2)| dump_value(v1) + " = " + &dump_value(v2))
				.collect::<Vec<_>>()
				.connect("\n            ") + "\n        ]",
        _ => "".to_string()
    }
}

fn dump_value_new_table(v: &Value) -> String {
    if options::run.trace_set {
    	dump_value_table(v)
    } else {
    	dump_value(v)
    }
}

/* Normal "print" uses this */
fn dump_value_for_user(v: &Value) -> String {
    match *v {
        Value::StringValue (s) | Value::AtomValue (s) => s,
        _ => dump_value(v)
    }
}

/* --- repl.ml helper printers --- */

/* FIXME: Can all these various display functions be condensed at all? */
/* Also, shouldn't more of this be exposed to code? */

/* Should the REPL should show a key/value pair? if not hide it. */
fn should_show_item(&(k, _): &(&Value, &Value)) -> bool {
    !vec![Value::PARENT_KEY, Value::HAS_KEY, Value::SET_KEY, Value::LET_KEY].iter().any(|x| x == k)
}

/* Sort items in objects/tables by key name */
fn sort_items(&(k1, v1): &(&Value, &Value), &(k2, v2): &(&Value, &Value)) -> Ordering {
    match (*k1, *k2) {
		(Value::AtomValue (s1), Value::AtomValue (s2)) => s1.cmp(s2),
		(Value::AtomValue (_), _) => Ordering::Less,
		(_, Value::AtomValue (_)) => Ordering::Greater,
		(Value::FloatValue (n1), Value::FloatValue (n2)) => n1.cmp(n2),
		_ => Ordering::Equal
	}
}

/* Display the key atom -- special-cased to avoid using a dot, since defns don't use them */
fn display_key(k: &Value) -> String {
    match *k {
		Value::AtomValue (s) => s,
		Value::FloatValue (n) => format!("<{}>", n.to_string()),
		_ => "<error>".to_string()
	}
}

/* (Optionally) truncate a string and append a suffix */
fn truncate(mut s: String, limitAt: usize, reduceTo: usize, suffix: &str) -> String {
    if s.len() > limitAt {
    	s.truncate(reduceTo) + suffix
    }
    else { s }
}

/* Provide a compact view of tables/objects for the REPL */
fn display_table(t: &TableValue) -> String {
    let items = t.iter()
    	.collect::<Vec<_>>()
    	.sort_by(sort_items);
    let ordered = items.iter()
    	.filter(should_show_item);
    fn f(&(k, v): &(&Value, &Value)) -> String {
    	display_key(k) + " = " + &repl_display(v, false)
   	}
    let out = match ordered.map(f).collect::<Vec<_>>() {
        ref toks if toks.is_empty() => "[...]".to_string(),
        toks => format!("[{}; ...]", toks.connect("; "))
    };
    truncate(out, 74, 72, "...]")
}

/* Create a string representation of a value for the REPL */
fn repl_display(value: &Value, recurse: bool) -> String {
    match *value {
        Value::Null => "null".to_string(),
        Value::True => "true".to_string(),
        Value::FloatValue (n) => n.to_string(),
        Value::StringValue (s) => escape_string(s),
        Value::AtomValue (s) => format!(".{}", s),
        Value::TableValue (ref t) | Value::ObjectValue (ref t) =>
            if recurse { display_table(t) } else { "<object>".to_string() },
        _ => dump_value_for_user(value)
    }
}
