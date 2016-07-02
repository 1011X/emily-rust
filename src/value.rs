/* Data representation for a runtime value. */

use std::collections::HashMap;
use std::hash::Hasher;
use std::borrow::Cow;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
#[derive(Clone)]
pub struct ClosureExecUser {
	body      : token::CodeSequence,
	scoped    : bool,  /* Should the closure execution get its own let scope? */
	env_scope : Value, /* Captured scope environment of closure manufacture */
	/* Another option would be to make the "new" scope early & excise 'key': */
	key       : Vec<String>, /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
	has_return: bool,  /* Should the closure execution get its own "return" continuation? */
}

#[derive(Clone)]
pub enum ClosureExec {
	User(ClosureExecUser),
	Builtin(Box<Fn(Vec<Value>) -> Value>)
}

#[derive(Clone)]
pub enum ClosureThis {
	Blank,                  /* Newly born closure */
	Never,                  /* Closure is not a method and should not receive a this. */
	Current(Value, Value),  /* Closure is a method, has a provisional current/this. */
	Frozen(Value, Value),   /* Closure is a method, has a final, assigned current/this. */
}

pub struct ClosureValue {
	exec: ClosureExec,
	need_args: usize,      /* Count this down as more values are added to bound */
	bound: Vec<Value>,     /* Already-curried values -- BACKWARD, first application last */
	this: ClosureThis,     /* Tracks the "current" and "this" bindings */
}

#[derive(Eq)]
pub enum Value<'a> {
    /* "Primitive" values */
	Null,
	True,
	Float(f64),
	String(String),
	Atom(Cow<'a, str>),

	/* Hack types for builtins */ /* FIXME: Can some of these be deprecated? */
	BuiltinFunction           (Box<Fn(Value) -> Value>), /* function argument = result */
	BuiltinUnaryMethod        (Box<Fn(Value) -> Value>), /* function self = result */
	BuiltinMethod(Box<Fn(Value) -> Fn(Value) -> Value>), /* function self argument = result */
	BuiltinHandoff (Box<Fn(ExecuteContext) -> Fn(ExecuteStack) -> Fn(Value, token::CodePosition) -> Value>), /* Take control of interpreter */

    /* Complex user-created values */
    
	/* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
	
	Closure(ClosureValue),
	UserMethod(Box<Value>),
	Table(TableValue),
	Object(TableValue), /* Same as Value::Table but treats 'this' different */
	Continuation(ExecuteStack, token::CodePosition), /* CodePosition only needed for traceback */
}

// Used for implementing OCaml's equality rules
// Note: PartialEq should not be implemented like this. This is just temporary. Maybe.
impl PartialEq for Value {
	fn eq(&self, other: &Value) -> bool {
		match (self, other) {
			(&Value::Null, &Value::Null)
			| (&Value::True, &Value::True)
				=> true,
			
			(&Value::Float(f1), &Value::Float(f2)) if f1.is_nan() && f2.is_nan()
				=> true,
			
			_ => self as *const Value == other as *const Value,
		}
	}
}

impl Hash for Value {
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		// TODO: implement
		match self {
			Value::Null => hasher.write_u8(0),
			Value::True => hasher.write_u8(1),
			Value::Float(f) => {
				hasher.write_u8(2);
				hasher.write_u64(f as u64);
			}
			Value::String(ref s) => {
				hasher.write_u8(3);
				
			}
		}
	}
}

impl Clone for Value {
	
}

// Thoughts: implementation should be in Display, and ToString should use THAT to get
// its value. Right now it's backwards, since it's more convenient because current
// implementation will have a String object already allocated, so we can just use that.
impl ToString for Value {
	fn to_string(&self) -> String {
		if options::RUN.track_objects {
			pretty::dump_value_tree_general(pretty::label_wrapper, v)
		}
		else {
			pretty::dump_value_tree_general(pretty::simple_wrapper, v)
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter) {
		f.write_str(&self.to_string());
	}
}

/* The "registers" are values 1 and 2 described in execute.rs comments */
/* The CodePositions are (1) the root of the current group (2) the symbol yielding "value 2" */
pub enum RegisterState {
	LineStart(Value, token::CodePosition),
	FirstValue(Value, token::CodePosition, token::CodePosition),
	PairValue(Value, Value, token::CodePosition, token::CodePosition)
}

/* Each frame on the stack has the two value "registers" and a codeSequence reference which
   is effectively an instruction pointer. */
pub struct ExecuteFrame {
    register: RegisterState,
    code:     token::CodeSequence,
    scope:    Value,
}

/* The current state of an execution thread consists of just the stack of frames. */
pub type ExecuteStack = Vec<ExecuteFrame>;

pub struct ExecuteContext {
    null_proto   : Value,
    true_proto   : Value,
    float_proto  : Value,
    string_proto : Value,
    atom_proto   : Value,
    object_proto : Value,
}

#[derive(Clone, Copy)]
pub enum TableBlankKind {
	TrueBlank, /* Really, actually empty. Only used for snippet scopes. */
	NoSet,     /* Has .has. Used for immutable builtin prototypes. */
	NoLet,     /* Has .set. Used for "flat" expression groups. */
	WithLet,   /* Has .let. Used for scoped groups. */
}

/* For making a scope inside an object literal */
pub enum TableBoxKind {
	BoxNew(token::BoxKind),
	BoxValue(Value),
}

pub struct ExecuteStarter {
	root_scope: Value,
	context: ExecuteContext,
}

pub static mut ID_GENERATOR: usize = 0;

/* "Keywords" */

macro_rules! keywords {
	($($name:ident, $name_string:ident = $s:expr;)*) => {
		$(
			pub static $name_string : &'static str = $s;
			pub static $name : Value = Value::Atom(Cow::Borrowed($name_string));
		)*
	};
}

keywords! {
	HAS_KEY,        HAS_KEY_STRING        = "has";
	SET_KEY,        SET_KEY_STRING        = "set";
	LET_KEY,        LET_KEY_STRING        = "let";
	PARENT_KEY,     PARENT_KEY_STRING     = "parent";
	ID_KEY,         ID_KEY_STRING         = "!id";
	CURRENT_KEY,    CURRENT_KEY_STRING    = "current";
	THIS_KEY,       THIS_KEY_STRING       = "this";
	SUPER_KEY,      SUPER_KEY_STRING      = "super";
	RETURN_KEY,     RETURN_KEY_STRING     = "return";
	PACKAGE_KEY,    PACKAGE_KEY_STRING    = "package";
	PROJECT_KEY,    PROJECT_KEY_STRING    = "project";
	DIRECTORY_KEY,  DIRECTORY_KEY_STRING  = "directory";
	INTERNAL_KEY,   INTERNAL_KEY_STRING   = "internal";
	NONLOCAL_KEY,   NONLOCAL_KEY_STRING   = "nonlocal";
	PRIVATE_KEY,    PRIVATE_KEY_STRING    = "private";
	EXPORT_LET_KEY, EXPORT_LET_KEY_STRING = "exportLet";
}

// Really needed?
pub fn table_set_option(table: &mut TableValue, key: Value, value: Option<Value>) {
	if let Some(v) = value {
		table.insert(key, v);
	}
}

pub fn table_from(value: Value) -> Result<TableValue, ocaml::Failure> {
	match value {
		Value::Table(v) |
		Value::Object(v) => Ok(v),
		
		_ => Err(ocaml::Failure("Internal error-- interpreter accidentally treated a non-object as an object in a place this should have been impossible.".to_string()))
	}
}
