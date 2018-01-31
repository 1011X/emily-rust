/* Pretty-printers for types from various other files */

use std::cmp::Ordering;
use std::borrow::Cow;

use token::{
    //CodeSequence,
    Token,
    TokenContents,
    //TokenGroup,
    //TokenGroupKind,
};
//use options;
use value::{
    self,
    TableValue,
    Value,
    //ClosureExec,
    //ClosureValue,
    RegisterState,
};


/* --- Code printers --- */

/* "Disassemble" a token tree into a human-readable string (specializable) */
fn dump_code_tree_general<'tc>(
	group_printer: fn(&Token, String, String, /*CodeSequence*/ ()) -> String,
	token: &Token<'tc>)
-> Cow<'tc, str>
{
    match token.contents {
    /* For a simple (nongrouping) token, return a string for just the item */
        TokenContents::Word(ref x)
        | TokenContents::Symbol(ref x) => x.clone(),
        TokenContents::String(ref x) => Cow::from(format!("\"{}\"", x)),
        TokenContents::Atom(ref x) => Cow::from(format!(".{}", x)),
        TokenContents::Number(x) => Cow::from(x.to_string()),
        /*
        TokenContents::Group(TokenGroup {kind, closure, items}) => {
            let (l, r) = match kind {
                TokenGroupKind::Plain => ("(", ")"),
                TokenGroupKind::Scoped => ("{", "}"),
                TokenGroupKind::Box(_) => ("[", "]"),
            };
            
            let l = match closure {
                TokenGroupKind::NonClosure => String::new(),
                TokenGroupKind::ClosureWithBinding(_, ref binding) => 
                    format!("^{}", binding.join(" ")),
            } + l;
            
            /* GroupPrinter is an argument function which takes the left group symbol, right group
               symbol, and group contents, and decides how to format them all. */
            Cow::from(group_printer(token, l, r.to_string(), items))
        }
        */
    }
}

/* "Disassemble" a token tree into a human-readable string (specialized for looking like code) */
/*
pub fn dump_code_tree_terse(token: &Token) -> Cow<'static, str> {
    fn group_printer(
    	token: &Token,
    	mut l: String,
    	r: String,
    	items: /*CodeSequence*/ ())
    	-> String
	{
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
pub fn dump_code_tree_dense(token: &Token) -> Cow<'static, str> {
    fn one_token(x: &Token) -> String {
        format!("{} {}", x.at, dump_code_tree_general(group_printer, x))
    }
    
    fn group_printer(token: &Token, l: String, r: String, items: /*CodeSequence*/ ()) -> String {
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
*/

/* --- Value printers --- */

/* Re-escape string according to the Emily reader's rules */
// maybe this could be replaced with escape_default() ?
fn escape_string(s: &str) -> String {
    let mut sb = String::with_capacity(s.len() + 2); // best guess
    
    sb.push('"');
    
    for c in s.chars() {
        match c {
            '"' | '\\' => {
                sb.push('\\');
                sb.push(c);
            }
            '\n' => sb += "\\n",
            _ => sb.push(c),
        }
    }
    
    sb.push('"');
    
    sb
}

fn angle_wrap(s: &str) -> String { format!("<{}>", s) }

fn id_string_for_table(t: &TableValue) -> Cow<'static, str> {
    match t.get(&value::ID_KEY) {
        None => Cow::from("UNKNOWN"),
        Some(&Value::Float(v)) => Cow::from((v as i32).to_string()),
        _ => Cow::from("INVALID") /* Should be impossible */
    }
}

fn id_string_for_value(v: &Value) -> Cow<'static, str> {
    match *v {
        Value::Table(ref t)
        | Value::Object(ref t) =>
            id_string_for_table(t),
        
        _ => Cow::from("UNTABLE")
    }
}

pub fn dump_value_tree_general(
	wrapper: fn(&'static str, &Value) -> String,
	v: &Value)
	-> Cow<'static, str>
{
    match *v {
        Value::Null => Cow::from("<null>"),
        Value::True => Cow::from("<true>"),
        Value::Float(v) => Cow::from(v.to_string()),
        Value::String(ref s) => Cow::from(escape_string(s)),
        Value::Atom(ref s) => Cow::from(format!(".{}", s)),
        Value::BuiltinFunction(_) => Cow::from("<builtin>"),
        /*
        Value::BuiltinMethod(_) => Cow::from("<object-builtin>"),
        Value::BuiltinUnaryMethod(_) => Cow::from("<property-builtin>"),
        Value::Closure(ClosureValue {exec: e, need_args: n}) => {
            let tag = match e {
                ClosureExec::User(_) => "closure",
                ClosureExec::Builtin(_) => "closure-builtin",
            };
            
            Cow::from(format!("<{}/{}>", tag, n))
        }
        */
        Value::Table(_) => Cow::from(wrapper("scope", v)), /* From the user's perspective, a table is a scope */
        Value::Object(_) => Cow::from(wrapper("object", v)),
        //Value::Continuation(_) => Cow::from("<return>"),
    }
}

pub fn simple_wrapper(label: &str, _: &Value) -> String {
    angle_wrap(label)
}

pub fn label_wrapper(label: &str, obj: &Value) -> String {
    match *obj {
        Value::Table(ref t)
        | Value::Object(ref t) =>
            format!("<{}:{}>", label, id_string_for_table(t)),
        
        _ => angle_wrap(label),
    }
}

/* FIXME: The formatting here is not even a little bit generalized. */

fn dump_value_unwrapped_table(t: &TableValue) -> String {
    " = [\n            ".to_owned()
    + &t.iter()
        .map(|(v1, v2)| format!("{} = {}", v1, v2))
        .collect::<Vec<String>>()
        .join("\n            ")
    + "\n        ]"
}

fn dump_value_table(v: &Value) -> String {
	let unwrapped = match *v {
        Value::Table(ref t) | Value::Object(ref t)
            => dump_value_unwrapped_table(t),
        
        _ => String::new()
    };
    v.to_string() + &unwrapped
}

pub fn dump_value_new_table(v: &Value) -> String {
	/*
	if unsafe { options::RUN.trace_set } {
	    dump_value_table(v)
	} else {
	    v.to_string()
	}
	*/
	v.to_string()
}

/* Normal "print" uses this */
pub fn dump_value_for_user(v: &Value) -> Cow<str> {
    match *v {
        Value::String(ref s) => Cow::Borrowed(s),
        Value::Atom(ref s) => s.clone(),
        _ => Cow::from(v.to_string())
    }
}

/* --- repl.ml helper printers --- */

/* FIXME: Can all these various display functions be condensed at all? */
/* Also, shouldn't more of this be exposed to code? */

/* Should the REPL show a key/value pair? if not hide it. */
fn should_show_item(&&(k, _): &&(&Value, &Value)) -> bool {
	use super::value::*;
    [&*PARENT_KEY, &*HAS_KEY, &*SET_KEY, &*LET_KEY].contains(&k)
    
}

/* Sort items in objects/tables by key name */
fn sort_items(&(k1, v1): &(&Value, &Value), &(k2, v2): &(&Value, &Value)) -> Ordering {
	use self::Value::*;
    match (k1, k2) {
        (&Atom(ref s1), &Atom(ref s2)) => s1.cmp(&s2),
        (&Atom(_), _) => Ordering::Less,
        (_, &Atom(_)) => Ordering::Greater,
        (&Float(n1), &Float(n2))
        	=> n1.partial_cmp(&n2)
        	.unwrap_or(Ordering::Less),
        _ => Ordering::Equal,
    }
}

/* Display the key atom -- special-cased to avoid using a dot, since defns don't use them */
fn display_key(k: &Value) -> Cow<str> {
    match *k {
        Value::Atom(ref s) => s.clone(),
        Value::Float(n) => Cow::from(format!("<{}>", n)),
        _ => Cow::from("<error>")
    }
}

/* (Optionally) truncate a string and append a suffix */
fn truncate(mut s: String, limit_at: usize, reduce_to: usize, suffix: &str) -> String {
    if s.len() > limit_at {
        s.truncate(reduce_to);
        s + suffix
    } else {
    	s
    }
}

/* Provide a compact view of tables/objects for the REPL */
fn display_table(t: &TableValue) -> String {
    let mut items = t.iter().collect::<Vec<_>>();
    items.sort_by(sort_items);
    
    let ordered = items.iter()
        .filter(should_show_item);
    
    let f = |&(k, v)| display_key(k).into_owned() + " = " + &repl_display(v, false);
    
    let out = match *ordered.map(f).collect::<Vec<_>>() {
        [] => "[...]".to_owned(),
        [ref toks..] => format!("[{}; ...]", toks.join("; ")),
    };
    
    truncate(out, 74, 72, "...]")
}

/* Create a string representation of a value for the REPL */
pub fn repl_display(value: &Value, recurse: bool) -> Cow<str> {
    match *value {
        Value::Null => Cow::from("null"),
        Value::True => Cow::from("true"),
        
        Value::Float(n) => Cow::from(n.to_string()),
        Value::String(ref s) => Cow::from(escape_string(s)),
        Value::Atom(ref s) => Cow::from(format!(".{}", s)),
        Value::Table(ref t)
        | Value::Object(ref t) =>
		    if recurse {
		    	Cow::from(display_table(t))
			} else {
				Cow::from("<object>")
			},
		
        _ => dump_value_for_user(value)
    }
}
