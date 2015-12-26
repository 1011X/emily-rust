/* Data representation for a runtime value. */

#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use std::hash::Hasher;

use token;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
#[derive(Clone)]
pub struct ClosureExecUser {
	body       : token::CodeSequence,
	scoped     : bool,  /* Should the closure execution get its own let scope? */
	env_scope  : Box<Value>, /* Captured scope environment of closure manufacture */
	/* Another option would be to make the "new" scope early & excise 'key': */
	key        : Vec<String>, /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
	has_return : bool    /* Should the closure execution get its own "return" continuation? */
}

#[derive(Clone)]
pub enum ClosureExec {
	User (ClosureExecUser),
	Builtin (Box<Fn(Vec<Value>) -> Value>)
}

#[derive(Clone)]
pub enum ClosureThis {
	Blank,     /* Newly born closure */
	Never,     /* Closure is not a method and should not receive a this. */
	Current (Box<Value>, Box<Value>), /* Closure is a method, has a provisional current/this. */
	Frozen (Box<Value>, Box<Value>),  /* Closure is a method, has a final, assigned current/this. */
}

pub struct ClosureValue {
	exec     : ClosureExec,
	need_args: usize,      /* Count this down as more values are added to bound */
	bound    : Vec<Value>, /* Already-curried values -- BACKWARD, first application last */
	this     : ClosureThis, /* Tracks the "current" and "this" bindings */
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
	Closure (ClosureValue),
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

pub struct ExecuteStarter {
	root_scope: Value,
	context: ExecuteContext,
}

pub static mut ID_GENERATOR: f64 = 0.0;

/* "Keywords" */
lazy_static! {
	pub static ref HAS_KEY_STRING        : String = "has".to_string();
	pub static ref HAS_KEY               : Value  = Value::Atom (HAS_KEY_STRING);
	pub static ref SET_KEY_STRING        : String = "set".to_string();
	pub static ref SET_KEY               : Value  = Value::Atom (SET_KEY_STRING);
	pub static ref LET_KEY_STRING        : String = "let".to_string();
	pub static ref LET_KEY               : Value  = Value::Atom (LET_KEY_STRING);
	pub static ref PARENT_KEY_STRING     : String = "parent".to_string();
	pub static ref PARENT_KEY            : Value  = Value::Atom (PARENT_KEY_STRING);
	pub static ref ID_KEY_STRING         : String = "!id".to_string();
	pub static ref ID_KEY                : Value  = Value::Atom (ID_KEY_STRING);
	pub static ref CURRENT_KEY_STRING    : String = "current".to_string();
	pub static ref CURRENT_KEY           : Value  = Value::Atom (CURRENT_KEY_STRING);
	pub static ref THIS_KEY_STRING       : String = "this".to_string();
	pub static ref THIS_KEY              : Value  = Value::Atom (THIS_KEY_STRING);
	pub static ref SUPER_KEY_STRING      : String = "super".to_string();
	pub static ref SUPER_KEY             : Value  = Value::Atom (SUPER_KEY_STRING);
	pub static ref RETURN_KEY_STRING     : String = "return".to_string();
	pub static ref RETURN_KEY            : Value  = Value::Atom (RETURN_KEY_STRING);
	pub static ref PACKAGE_KEY_STRING    : String = "package".to_string();
	pub static ref PACKAGE_KEY           : Value  = Value::Atom (PACKAGE_KEY_STRING);
	pub static ref PROJECT_KEY_STRING    : String = "project".to_string();
	pub static ref PROJECT_KEY           : Value  = Value::Atom (PROJECT_KEY_STRING);
	pub static ref DIRECTORY_KEY_STRING  : String = "directory".to_string();
	pub static ref DIRECTORY_KEY         : Value  = Value::Atom (DIRECTORY_KEY_STRING);
	pub static ref INTERNAL_KEY_STRING   : String = "internal".to_string();
	pub static ref INTERNAL_KEY          : Value  = Value::Atom (INTERNAL_KEY_STRING);
	pub static ref NONLOCAL_KEY_STRING   : String = "nonlocal".to_string();
	pub static ref NONLOCAL_KEY          : Value  = Value::Atom (NONLOCAL_KEY_STRING);
	pub static ref PRIVATE_KEY_STRING    : String = "private".to_string();
	pub static ref PRIVATE_KEY           : Value  = Value::Atom (PRIVATE_KEY_STRING);
	pub static ref EXPORT_LET_KEY_STRING : String = "exportLet".to_string();
	pub static ref EXPORT_LET_KEY        : Value  = Value::Atom (EXPORT_LET_KEY_STRING);
}

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
	if let Some(v) = value {
		table.insert(key, v);
	}
}

pub fn table_from(value: Value) -> Result<TableValue, ocaml::Failure> {
	match value {
		Value::Table (v) | Value::Object (v) => Ok(v),
		_ => Err(ocaml::Failure("Internal error-- interpreter accidentally treated a non-object as an object in a place this should have been impossible.".to_string()))
	}
}
