/* Data representation for a runtime value. */

use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::borrow::Cow;
use std::sync::atomic;

//use ocaml;
use token;
use pretty;
//use options;

pub type TableValue = HashMap<Value, Value>;
/*
/* Closure types: */
#[derive(Clone)]
pub struct ClosureExecUser {
    body      : token::CodeSequence,
    scoped    : bool,  /* Should the closure execution get its own let scope? */
    env_scope : Box<Value>, /* Captured scope environment of closure manufacture */
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
    Current(Box<Value>, Box<Value>),  /* Closure is a method, has a provisional current/this. */
    Frozen(Box<Value>, Box<Value>),   /* Closure is a method, has a final, assigned current/this. */
}

pub struct ClosureValue {
    exec: ClosureExec,
    need_args: usize,      /* Count this down as more values are added to bound */
    bound: Vec<Value>,     /* Already-curried values -- BACKWARD, first application last */
    this: ClosureThis,     /* Tracks the "current" and "this" bindings */
}
*/
#[derive(Clone)]
pub enum Value {
    /* "Primitive" values */
    Null,
    True,
    Float(f64),
    String(String),
    Atom(Cow<'static, str>),
    
    /* Hack types for builtins */
    /* FIXME: Can some of these be deprecated? */
    BuiltinFunction(fn(Value) -> Value), /* function argument = result */
	/*
    BuiltinUnaryMethod(Box<Fn(Value) -> Value>), /* function self = result */
    BuiltinMethod(Box<Fn(Value) -> Fn(Value) -> Value>), /* function self argument = result */
    BuiltinHandoff(Box<Fn(ExecuteContext) -> Fn(ExecuteStack) -> Fn(Value, token::CodePosition) -> Value>), /* Take control of interpreter */

    /* Complex user-created values */
    
    /* Is this getting kind of complicated? Should curry be wrapped closures? Should callcc be separate? */
    Closure(ClosureValue),
    UserMethod(Box<Value>),
    */
    Table(TableValue),
    Object(TableValue), /* Same as Value::Table but treats 'this' different */
    //Continuation(ExecuteStack, token::CodePosition), /* CodePosition only needed for traceback */
}

// Used for implementing OCaml's equality rules
// Note: PartialEq probably shouldn't be implemented like this. This is just temporary. Maybe.
impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
    	use self::Value::*;
        match (self, other) {
            (&Null, &Null)
            | (&True, &True)
                => true,
            
            (&Float(f1), &Float(f2))
                => f1 == f2 || f1.is_nan() && f2.is_nan(),
            
            (&String(ref s1), &String(ref s2))
            	=> s1 == s2,
        	
        	(&Atom(ref s1), &Atom(ref s2))
        		=> s1 == s2,
    		
    		(&Table(ref t1), &Table(ref t2))
    		| (&Object(ref t1), &Object(ref t2))
    			=> t1 == t2,
            
            (&BuiltinFunction(fn1), &BuiltinFunction(fn2))
            	=> fn1 == fn2,
        	
        	_ => false
            //_ => self as *const Value == other as *const Value,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        use self::Value::*;
        ::std::mem::discriminant(self).hash(hasher);
        match *self {
            Null | True   => {}
            Float(f)      => f.to_bits().hash(hasher),
            String(ref s) => s.hash(hasher),
            Atom(ref c)   => c.hash(hasher),
            
            Table(ref t) | Object(ref t)
            	=> (t as *const _).hash(hasher),
        	BuiltinFunction(f)
        		=> f.hash(hasher),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let wrapper = pretty::simple_wrapper;
            //if unsafe {options::RUN.track_objects} { pretty::label_wrapper }
            //else { pretty::simple_wrapper };
        
        f.write_str(&pretty::dump_value_tree_general(wrapper, self))
    }
}

/* The "registers" are values 1 and 2 described in execute.rs comments */
/* The CodePositions are (1) the root of the current group (2) the symbol yielding "value 2" */
#[derive(Clone)]
pub enum RegisterState {
    LineStart(Value, token::CodePosition),
    FirstValue(Value, token::CodePosition, token::CodePosition),
    PairValue(Value, Value, token::CodePosition, token::CodePosition)
}

impl fmt::Display for RegisterState {
    /* Pretty print for RegisterState. */
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RegisterState::LineStart(ref v, _) =>
                write!(f, "LineStart:{}", v),
        
            RegisterState::FirstValue(ref v, _, _) =>
                write!(f, "FirstValue:{}", v),
        
            RegisterState::PairValue(ref v1, ref v2, _, _) =>
                write!(f, "PairValue:{},{}", v1, v2),
        }
    }
}
/*
/* Each frame on the stack has the two value "registers" and a codeSequence reference which
   is effectively an instruction pointer. */
pub struct ExecuteFrame {
    register: RegisterState,
    code:     token::CodeSequence,
    scope:    Value,
}

/* The current state of an execution thread consists of just the stack of frames. */
pub type ExecuteStack = Vec<ExecuteFrame>;
*/
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
/*
/* For making a scope inside an object literal */
pub enum TableBoxKind {
    BoxNew(token::BoxKind),
    BoxValue(Value),
}
*/
pub struct ExecuteStarter {
    root_scope: Value,
    context: ExecuteContext,
}

pub static ID_GENERATOR: atomic::AtomicUsize = atomic::ATOMIC_USIZE_INIT;

/* "Keywords" */

macro_rules! keywords {
    ($($name:ident, $name_str:ident = $s:expr;)*) => {
        $(pub static $name_str: &str = $s;)*
        
        lazy_static! {
            $(pub static ref $name: Value = Value::Atom(Cow::Borrowed($name_str));)*
        }
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

pub fn table_from(value: Value) -> TableValue {
    match value {
        Value::Table(v)
        | Value::Object(v) => v,
        
        _ => panic!("Internal error: interpreter accidentally treated a non-object as an object in a place this should have been impossible.")
    }
}
