/* Data representation for a runtime value. */

use std::collections::HashMap;
use std::hash::Hasher;
use token;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
#[derive(Clone)]
pub enum ClosureExec {
	// XXX: ClosureExecUser, struct ClosureExecUser
	User {
		body       : token::CodeSequence,
		scoped     : bool,  /* Should the closure execution get its own let scope? */
		env_scope  : Box<Value>, /* Captured scope environment of closure manufacture */
		/* Another option would be to make the "new" scope early & excise 'key': */
		key        : Vec<String>, /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
		has_return : bool    /* Should the closure execution get its own "return" continuation? */
	},
	// XXX: ClosureExecBuiltin
	Builtin(Box<Fn(Vec<Value>) -> Value>)
}

#[derive(Clone)]
pub enum ClosureThis {
	Blank,     /* Newly born closure */
	Never,     /* Closure is not a method and should not receive a this. */
	Current(Box<Value>, Box<Value>), /* Closure is a method, has a provisional current/this. */
	Frozen(Box<Value>, Box<Value>)  /* Closure is a method, has a final, assigned current/this. */
}

#[derive(Clone, Eq)]
pub enum Value {
    /* "Primitive" values */
	Null,
	True,
	Float (f64),
	String (String),
	Atom (String),

	/* Hack types for builtins */ /* FIXME: Can some of these be deprecated? */
	BuiltinFunction            (Box<Fn(Value) -> Value>), /* function argument = result */
	BuiltinUnaryMethod         (Box<Fn(Value) -> Value>), /* function self = result */
	BuiltinMethod (Box<Fn(Value) -> Fn(Value) -> Value>), /* function self argument = result */
	BuiltinHandoff (Box<Fn(ExecuteContext) -> Fn(ExecuteStack) -> Fn(Value, token::CodePosition) -> Value>), /* Take control of interpreter */

    /* Complex user-created values */
    
	/* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
	Closure {
		exec       : ClosureExec,
		need_args  : usize,      /* Count this down as more values are added to bound */
		bound      : Vec<Value>,   /* Already-curried values -- BACKWARD, first application last */
		this       : ClosureThis   /* Tracks the "current" and "this" bindings */
	},
	UserMethod (Box<Value>),
	Table (Box<TableValue>),
	Object (Box<TableValue>), /* Same as Value::Table but treats 'this' different */
	Continuation (ExecuteStack, token::CodePosition), /* CodePosition only needed for traceback */
}

/* Used for implementing OCaml's equality rules
	Note: PartialEq should NOT be implemented like this. This is just temporary. Maybe. */
impl PartialEq for Value {
	fn eq(&self, other: &Value) -> bool {
		match (self, other) {
			(&Value::Null, &Value::Null) => true,
			(&Value::True, &Value::True) => true,
			_ => self as *const Value == other as *const Value
		}
	}
}

impl Hash for Value {
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		// TODO: implement
		unimplemented!()
	}
}

/* The "registers" are values 1 and 2 described in execute.rs comments */
/* The CodePositions are (1) the root of the current group (2) the symbol yielding "value 2" */
#[derive(Clone)]
pub enum RegisterState {
	LineStart (Value, token::CodePosition),
	FirstValue (Value, token::CodePosition, token::CodePosition),
	PairValue (Value, Value, token::CodePosition, token::CodePosition)
}

/* Each frame on the stack has the two value "registers" and a codeSequence reference which
   is effectively an instruction pointer. */
#[derive(Clone)]
pub struct ExecuteFrame {
    register : RegisterState,
    code : token::CodeSequence,
    scope: Value
}

/* The current state of an execution thread consists of just the stack of frames. */
pub type ExecuteStack = Vec<ExecuteFrame>;

#[derive(Clone)]
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
#[derive(Clone)]
pub enum TableBoxKind {
	BoxNew (token::BoxKind),
	BoxValue (Value)
}

pub static mut ID_GENERATOR : f64 = 0.0;

/* "Keywords" */
pub static HAS_KEY_STRING       : &'static str  = "has";
pub static HAS_KEY              : Value         = Value::Atom (HAS_KEY_STRING);
pub static SET_KEY_STRING       : &'static str  = "set";
pub static SET_KEY              : Value         = Value::Atom (SET_KEY_STRING);
pub static LET_KEY_STRING       : &'static str  = "let";
pub static LET_KEY              : Value         = Value::Atom (LET_KEY_STRING);
pub static PARENT_KEY_STRING    : &'static str  = "parent";
pub static PARENT_KEY           : Value         = Value::Atom (PARENT_KEY_STRING);
pub static ID_KEY_STRING        : &'static str  = "!id";
pub static ID_KEY               : Value         = Value::Atom (ID_KEY_STRING);
pub static CURRENT_KEY_STRING   : &'static str  = "current";
pub static CURRENT_KEY          : Value         = Value::Atom (CURRENT_KEY_STRING);
pub static THIS_KEY_STRING      : &'static str  = "this";
pub static THIS_KEY             : Value         = Value::Atom (THIS_KEY_STRING);
pub static SUPER_KEY_STRING     : &'static str  = "super";
pub static SUPER_KEY            : Value         = Value::Atom (SUPER_KEY_STRING);
pub static RETURN_KEY_STRING    : &'static str  = "return";
pub static RETURN_KEY           : Value         = Value::Atom (RETURN_KEY_STRING);
pub static PACKAGE_KEY_STRING   : &'static str  = "package";
pub static PACKAGE_KEY          : Value         = Value::Atom (PACKAGE_KEY_STRING);
pub static PROJECT_KEY_STRING   : &'static str  = "project";
pub static PROJECT_KEY          : Value         = Value::Atom (PROJECT_KEY_STRING);
pub static DIRECTORY_KEY_STRING : &'static str  = "directory";
pub static DIRECTORY_KEY        : Value         = Value::Atom (DIRECTORY_KEY_STRING);
pub static INTERNAL_KEY_STRING  : &'static str  = "internal";
pub static INTERNAL_KEY         : Value         = Value::Atom (INTERNAL_KEY_STRING);
pub static NONLOCAL_KEY_STRING  : &'static str  = "nonlocal";
pub static NONLOCAL_KEY         : Value         = Value::Atom (NONLOCAL_KEY_STRING);
pub static PRIVATE_KEY_STRING   : &'static str  = "private";
pub static PRIVATE_KEY          : Value         = Value::Atom (PRIVATE_KEY_STRING);


// Really needed?
pub fn table_get(table: &TableValue, key: &Value) -> Option<Value> {
	table.get(key).cloned()
}

pub fn table_set(table: &mut TableValue, key: Value, value: Value) {
	table.insert(key, value);
}

pub fn table_has(table: &mut TableValue, key: &Value) -> bool {
	table.contains_key(key)
}

pub fn table_set_string(table: &mut TableValue, key: String, value: Value) {
	table.insert(Value::Atom (key), value);
}

pub fn table_set_option(table: &mut TableValue, key: Value, value: Option<Value>) {
	if let Some(ref x) = value { table.insert(key, x.clone()); }
}

pub fn table_from(value: Value) -> Result<TableValue, &'static str> {
	match value {
		Value::TableValue(v) |
		Value::ObjectValue(v) => Ok(v),
		_ => Err("Internal error-- interpreter accidentally treated a non-object as an object in a place this should have been impossible.")
	}
}
