/* Data representation for a runtime value. */

mod token;

use std::collections::HashMap;
use Value::*;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
struct ClosureExecUser {
    body     : token::CodeSequence,
    scoped   : bool,  /* Should the closure execution get its own let scope? */
    envScope : Value, /* Captured scope environment of closure manufacture */
    /* Another option would be to make the "new" scope early & excise 'key': */
    key      : Vec<String>, /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
    hasReturn : bool    /* Should the closure execution get its own "return" continuation? */
}

enum ClosureExec {
	ClosureExecUser(ClosureExecUser),
	ClosureExecBuiltin(Fn(Vec<Value>) -> Value)

enum ClosureThis {
	ThisBlank,     /* Newly born closure */
	ThisNever,     /* Closure is not a method and should not receive a this. */
	CurrentThis(Value, Value), /* Closure is a method, has a provisional current/this. */
	FrozenThis(Value, Value)  /* Closure is a method, has a final, assigned current/this. */

/* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
pub struct ClosureValue {
	exec   : ClosureExec,
	needArgs : isize,      /* Count this down as more values are added to bound */
	bound  : Vec<Value>,   /* Already-curried values -- BACKWARD, first application last */
	this   : ClosureThis   /* Tracks the "current" and "this" bindings */
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum Value {
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

enum TableBlankKind {
	TrueBlank, /* Really, actually empty. Only used for snippet scopes. */
	NoSet,     /* Has .has. Used for immutable builtin prototypes. */
	NoLet,     /* Has .set. Used for "flat" expression groups. */
	WithLet,   /* Has .let. Used for scoped groups. */
	BoxFrom(token::BoxKind) /* Scope inside an object literal; argument is result-object .parent */
}

/* The "registers" are values 1 and 2 described in execute.rs comments */
/* The CodePositions are (1) the root of the current group (2) the symbol yielding "value 2" */
enum RegisterState {
	LineStart(Value, token::CodePosition),
	FirstValue(Value, token::CodePosition, token::CodePosition),
	PairValue(Value, Value, token::CodePosition, token::CodePosition)
}

/* Each frame on the stack has the two value "registers" and a codeSequence reference which
   is effectively an instruction pointer. */
struct ExecuteFrame {
    register : RegisterState,
    code : token::CodeSequence,
    scope: Value
}

/* The current state of an execution thread consists of just the stack of frames. */
type ExecuteStack = Vec<ExecuteFrame>;

static mut ID_GENERATOR : f64 = 0;

/* "Keywords" */
static HAS_KEY_STRING       : &str  = "has";
static HAS_KEY              : Value = AtomValue(hasKeyString);
static SET_KEY_STRING       : &str  = "set";
static SET_KEY              : Value = AtomValue(setKeyString);
static LET_KEY_STRING       : &str  = "let";
static LET_KEY              : Value = AtomValue(letKeyString);
static PARENT_KEY_STRING    : &str  = "parent";
static PARENT_KEY           : Value = AtomValue(parentKeyString);
static ID_KEY_STRING        : &str  = "!id";
static ID_KEY               : Value = AtomValue(idKeyString);
static CURRENT_KEY_STRING   : &str  = "current";
static CURRENT_KEY          : Value = AtomValue(currentKeyString);
static THIS_KEY_STRING      : &str  = "this";
static THIS_KEY             : Value = AtomValue(thisKeyString);
static SUPER_KEY_STRING     : &str  = "super";
static SUPER_KEY            : Value = AtomValue(superKeyString);
static RETURN_KEY_STRING    : &str  = "return";
static RETURN_KEY           : Value = AtomValue(returnKeyString);
static PACKAGE_KEY_STRING   : &str  = "package";
static PACKAGE_KEY          : Value = AtomValue(packageKeyString);
static PROJECT_KEY_STRING   : &str  = "project";
static PROJECT_KEY          : Value = AtomValue(projectKeyString);
static DIRECTORY_KEY_STRING : &str  = "directory";
static DIRECTORY_KEY        : Value = AtomValue(directoryKeyString);
static NONLOCAL_KEY_STRING  : &str  = "nonlocal";
static NONLOCAL_KEY         : Value = AtomValue(nonlocalKeyString);
static PRIVATE_KEY_STRING   : &str  = "private";
static PRIVATE_KEY          : Value = AtomValue(privateKeyString);


fn table_get(table: TableValue, key: Value) -> Value {
	table.get(key).cloned()
}

/* Quarantined
fn table_set(mut table: TableValue, key: Value, value: Value) {
	table.insert(key, value);
}

fn table_has(table: TableValue, key: Value) -> bool {
	table.contains_key(&key)
}
*/

fn table_set_string(mut table: TableValue, key: String, value: Value) {
	table.insert(AtomValue(key), value);
}


fn table_set_option(mut table: TableValue, key: Value, value: Option<Value>) {
	if let Some(x) = value { table.insert(key, x); }
}

fn table_from(value: Value) -> TableValue {
	match value {
		TableValue(v) | ObjectValue(v) => v,
		_ => panic!("Internal error-- interpreter accidentally treated a non-object as an object in a place this should have been impossible.")
	}
}
