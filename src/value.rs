/* Data representation for a runtime value. */

mod token;

use std::collections::HashMap;
use Value::*;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
pub enum ClosureExec {
	// XXX: ClosureExecUser, struct ClosureExecUser
	User {
		body       : token::CodeSequence,
		scoped     : bool,  /* Should the closure execution get its own let scope? */
		env_scope  : Value, /* Captured scope environment of closure manufacture */
		/* Another option would be to make the "new" scope early & excise 'key': */
		key        : Vec<String>, /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
		has_return : bool    /* Should the closure execution get its own "return" continuation? */
	},
	// XXX: ClosureExecBuiltin
	Builtin(Fn(Vec<Value>) -> Value)
}

pub enum ClosureThis {
	ThisBlank,     /* Newly born closure */
	ThisNever,     /* Closure is not a method and should not receive a this. */
	CurrentThis(Value, Value), /* Closure is a method, has a provisional current/this. */
	FrozenThis(Value, Value)  /* Closure is a method, has a final, assigned current/this. */
}

/* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
#[derive(Clone)]
struct ClosureValue {
	exec       : ClosureExec,
	need_args  : usize,      /* Count this down as more values are added to bound */
	bound      : Vec<Value>,   /* Already-curried values -- BACKWARD, first application last */
	this_value : ClosureThis   /* Tracks the "current" and "this" bindings */
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Value {
    /* "Primitive" values */
	Null,
	True,
	FloatValue(f64),
	StringValue(String),
	AtomValue(String),

	/* Hack types for builtins */
	BuiltinFunctionValue              (Fn(Value) -> Value), /* function argument = result */
	BuiltinUnaryMethodValue           (Fn(Value) -> Value), /* function self = result */
	BuiltinMethodValue   (Fn(Value) -> Fn(Value) -> Value), /* function self argument = result */

    /* Complex user-created values */
	ClosureValue(ClosureValue),
	TableValue(TableValue),
	ObjectValue(TableValue), /* Same as TableValue but treats 'this' different */
	ContinuationValue(ExecuteStack, token::CodePosition), /* CodePosition only needed for traceback */

    /* Package support */
/*  PackageValue(Vec<Value>), */
/*  PackageDirectory(String) */
}

pub enum TableBlankKind {
	TrueBlank, /* Really, actually empty. Only used for snippet scopes. */
	NoSet,     /* Has .has. Used for immutable builtin prototypes. */
	NoLet,     /* Has .set. Used for "flat" expression groups. */
	WithLet,   /* Has .let. Used for scoped groups. */
	BoxFrom(token::BoxKind) /* Scope inside an object literal; argument is result-object .parent */
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
    register : RegisterState,
    code : token::CodeSequence,
    scope: Value
}

/* The current state of an execution thread consists of just the stack of frames. */
pub type ExecuteStack = Vec<ExecuteFrame>;

pub static mut ID_GENERATOR : f64 = 0.0;

/* "Keywords" */
pub static HAS_KEY_STRING       : &'static str  = "has";
pub static HAS_KEY              : Value         = AtomValue(HAS_KEY_STRING);
pub static SET_KEY_STRING       : &'static str  = "set";
pub static SET_KEY              : Value         = AtomValue(SET_KEY_STRING);
pub static LET_KEY_STRING       : &'static str  = "let";
pub static LET_KEY              : Value         = AtomValue(LET_KEY_STRING);
pub static PARENT_KEY_STRING    : &'static str  = "parent";
pub static PARENT_KEY           : Value         = AtomValue(PARENT_KEY_STRING);
pub static ID_KEY_STRING        : &'static str  = "!id";
pub static ID_KEY               : Value         = AtomValue(ID_KEY_STRING);
pub static CURRENT_KEY_STRING   : &'static str  = "current";
pub static CURRENT_KEY          : Value         = AtomValue(CURRENT_KEY_STRING);
pub static THIS_KEY_STRING      : &'static str  = "this";
pub static THIS_KEY             : Value         = AtomValue(THIS_KEY_STRING);
pub static SUPER_KEY_STRING     : &'static str  = "super";
pub static SUPER_KEY            : Value         = AtomValue(SUPER_KEY_STRING);
pub static RETURN_KEY_STRING    : &'static str  = "return";
pub static RETURN_KEY           : Value         = AtomValue(RETURN_KEY_STRING);
pub static PACKAGE_KEY_STRING   : &'static str  = "package";
pub static PACKAGE_KEY          : Value         = AtomValue(PACKAGE_KEY_STRING);
pub static PROJECT_KEY_STRING   : &'static str  = "project";
pub static PROJECT_KEY          : Value         = AtomValue(PROJECT_KEY_STRING);
pub static DIRECTORY_KEY_STRING : &'static str  = "directory";
pub static DIRECTORY_KEY        : Value         = AtomValue(DIRECTORY_KEY_STRING);
pub static NONLOCAL_KEY_STRING  : &'static str  = "nonlocal";
pub static NONLOCAL_KEY         : Value         = AtomValue(NONLOCAL_KEY_STRING);
pub static PRIVATE_KEY_STRING   : &'static str  = "private";
pub static PRIVATE_KEY          : Value         = AtomValue(PRIVATE_KEY_STRING);


pub fn table_get(table: TableValue, key: Value) -> Value {
	table.get(key).cloned()
}

/* Quarantined
pub fn table_set(mut table: TableValue, key: Value, value: Value) {
	table.insert(key, value);
}

pub fn table_has(table: TableValue, key: Value) -> bool {
	table.contains_key(&key)
}
*/

pub fn table_set_string(mut table: TableValue, key: String, value: Value) {
	table.insert(AtomValue(key), value);
}


pub fn table_set_option(mut table: TableValue, key: Value, value: Option<Value>) {
	if let Some(x) = value { table.insert(key, x); }
}

pub fn table_from(value: Value) -> TableValue {
	match value {
		TableValue(v) | ObjectValue(v) => v,
		_ => panic!("Internal error-- interpreter accidentally treated a non-object as an object in a place this should have been impossible.")
	}
}
