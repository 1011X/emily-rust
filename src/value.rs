/* Data representation for a runtime value. */

use std::collections::HashMap;
use std::hash::Hasher;
use std::borrow::Cow;

pub type TableValue = HashMap<Value, Value>;

/* Closure types: */
#[derive(Clone)]
pub struct ClosureExecUser {
	body: token::CodeSequence,
	scoped: bool,             /* Should the closure execution get its own let scope? */
	env_scope: Value,         /* Captured scope environment of closure manufacture */
	/* Another option would be to make the "new" scope early & excise 'key': */
	key: Vec<String>,         /* Not-yet-curried keys, or [] as special for "this is nullary" -- BACKWARD, first-applied key is last */
	has_return: bool          /* Should the closure execution get its own "return" continuation? */
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
	BuiltinFunction            (Box<Fn(Value) -> Value>), /* function argument = result */
	BuiltinUnaryMethod         (Box<Fn(Value) -> Value>), /* function self = result */
	BuiltinMethod (Box<Fn(Value) -> Fn(Value) -> Value>), /* function self argument = result */
	BuiltinHandoff (Box<Fn(ExecuteContext) -> Fn(ExecuteStack) -> Fn(Value, token::CodePosition) -> Value>), /* Take control of interpreter */

    /* Complex user-created values */
    
	/* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
	
	// XXX Note to self: STOP TRYING TO INTEGRATE ClosureValue INTO Value!!!
	// It's more convenient if you just... don't.
	Closure(ClosureValue),
	UserMethod(Box<Value>),
	Table(TableValue),
	Object(TableValue), /* Same as Value::Table but treats 'this' different */
	Continuation(ExecuteStack, token::CodePosition), /* CodePosition only needed for traceback */
}

// Used for implementing OCaml's equality rules
// Note: PartialEq should NOT be implemented like this. This is just temporary. Maybe.
impl PartialEq for Value {
	fn eq(&self, other: &Value) -> bool {
		match (self, other) {
			(&Value::Null, &Value::Null) |
			(&Value::True, &Value::True) => true,
			
			_ => self as *const Value == other as *const Value,
		}
	}
}

impl Hash for Value {
	fn hash<H: Hasher>(&self, hasher: &mut H) {
		// TODO: implement
		unimplemented!()
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

pub static mut ID_GENERATOR: f64 = 0.0;

/* "Keywords" */
pub static HAS_KEY_STRING        : &'static str = "has";
pub static SET_KEY_STRING        : &'static str = "set";
pub static LET_KEY_STRING        : &'static str = "let";
pub static PARENT_KEY_STRING     : &'static str = "parent";
pub static ID_KEY_STRING         : &'static str = "!id";
pub static CURRENT_KEY_STRING    : &'static str = "current";
pub static THIS_KEY_STRING       : &'static str = "this";
pub static SUPER_KEY_STRING      : &'static str = "super";
pub static RETURN_KEY_STRING     : &'static str = "return";
pub static PACKAGE_KEY_STRING    : &'static str = "package";
pub static PROJECT_KEY_STRING    : &'static str = "project";
pub static DIRECTORY_KEY_STRING  : &'static str = "directory";
pub static INTERNAL_KEY_STRING   : &'static str = "internal";
pub static NONLOCAL_KEY_STRING   : &'static str = "nonlocal";
pub static PRIVATE_KEY_STRING    : &'static str = "private";
pub static EXPORT_LET_KEY_STRING : &'static str = "exportLet";

pub static HAS_KEY        : Value = Value::Atom(Cow::Borrowed(HAS_KEY_STRING));
pub static SET_KEY        : Value = Value::Atom(Cow::Borrowed(SET_KEY_STRING));
pub static LET_KEY        : Value = Value::Atom(Cow::Borrowed(LET_KEY_STRING));
pub static PARENT_KEY     : Value = Value::Atom(Cow::Borrowed(PARENT_KEY_STRING));
pub static ID_KEY         : Value = Value::Atom(Cow::Borrowed(ID_KEY_STRING));
pub static CURRENT_KEY    : Value = Value::Atom(Cow::Borrowed(CURRENT_KEY_STRING));
pub static THIS_KEY       : Value = Value::Atom(Cow::Borrowed(THIS_KEY_STRING));
pub static SUPER_KEY      : Value = Value::Atom(Cow::Borrowed(SUPER_KEY_STRING));
pub static RETURN_KEY     : Value = Value::Atom(Cow::Borrowed(RETURN_KEY_STRING));
pub static PACKAGE_KEY    : Value = Value::Atom(Cow::Borrowed(PACKAGE_KEY_STRING));
pub static PROJECT_KEY    : Value = Value::Atom(Cow::Borrowed(PROJECT_KEY_STRING));
pub static DIRECTORY_KEY  : Value = Value::Atom(Cow::Borrowed(DIRECTORY_KEY_STRING));
pub static INTERNAL_KEY   : Value = Value::Atom(Cow::Borrowed(INTERNAL_KEY_STRING));
pub static NONLOCAL_KEY   : Value = Value::Atom(Cow::Borrowed(NONLOCAL_KEY_STRING));
pub static PRIVATE_KEY    : Value = Value::Atom(Cow::Borrowed(PRIVATE_KEY_STRING));
pub static EXPORT_LET_KEY : Value = Value::Atom(Cow::Borrowed(EXPORT_LET_KEY_STRING));

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
