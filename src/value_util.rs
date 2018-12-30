/* This file contains support methods for creating values with certain properties, split out from Value for module recursion reasons. */

use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::value::*;
use crate::token::*;
use crate::options;
use crate::pretty;
//use crate::tokenize;

/* Misc failure throw methods */
fn bad_arg_table(name: &str, var: &Value) -> String {
    format!("Bad argument to {}: Need table, got {}", name, var)
}
/*
fn bad_arg_closure(name: &str, var: &Value) -> Result<!, String> {
    Err(format!("Bad argument to {}: Need closure, got {}", name, var))
}
*/
pub fn impossible_arg(name: &str) -> ! {
    unreachable!("Impossible argument to {}", name)
}


fn misapply_string(a: &Value, b: &Value) -> String {
    format!("Runtime failure: {} can't respond to {}", a, b)
}

/*
fn raw_misapply_arg(a: &Value, b: &Value) -> String {
    misapply_string(a, b)
}
*/

/* Tools */

/* Create a closure from a function */
pub fn snippet_closure(arg_count: usize, exec: fn(Vec<Value>) -> Result<Value, String>) -> Value {
    Value::Closure(ClosureValue {
        exec: ClosureExec::Builtin(exec),
        need_args: arg_count,
        bound: Vec::new(),
        this: ClosureThis::Never,
    })
}

/* For debugging, call this after creating a hashtable set to become a Value */
/*
fn seal_table(t: &mut TableValue) {
    use std::sync::atomic::Ordering;
    if options::RUN.read().unwrap().track_objects {
        let id = ID_GENERATOR.fetch_add(1, Ordering::SeqCst);
        t.insert(ID_KEY.clone(), Value::Float(id as f64));
    }
}
*/

/* Same as calling table_blank(TrueBlank). We need a separate version because
   table_blank relies on some of the functions that require table_true_blank
   to themselves be defined, and it gets awkward w/mutual recursion. */
fn table_true_blank() -> TableValue {
    let mut t = HashMap::new();
    //seal_table(&mut t);
    use std::sync::atomic::Ordering;
    if options::RUN.read().unwrap().track_objects {
        let id = ID_GENERATOR.fetch_add(1, Ordering::SeqCst);
        t.insert(ID_KEY.clone(), Value::Float(id as f64));
    }
    t
}
/*
fn table_true_blank_inheriting(v: Value) -> TableValue {
    let mut t = table_true_blank();
    t.insert(PARENT_KEY.clone(), v);
    t
}
*/
/* Makes a scope to be used in a snippet_text_closure */
fn snippet_scope(bindings: Vec<(String, Value)>) -> Value {
    let mut scope_table = table_true_blank();
    for (k, v) in bindings {
        scope_table.insert(Value::Atom(k), v);
    }
    Value::Table(scope_table)
}

/* Define an ad hoc function using a literal string inside the interpreter. */
fn snippet_text_closure_abstract(
    source: CodeSource,
    this_kind: ClosureThis,
    context: &[(&str, Value)],
    keys: &[&str],
    text: &str
) -> Value {
    Value::Closure(ClosureValue {
        exec: ClosureExec::User(ClosureExecUser {
            body: tokenize::snippet(source, text.to_owned()),
            env_scope: box snippet_scope(context.iter()
                .map(|(s,v)| (s.to_string(), v.clone()))
                .collect()
            ),
            scoped: false,
            key: keys.iter().map(|s| s.to_string()).collect(),
            has_return: false,
        }),
        need_args: keys.len(),
        bound: Vec::new(),
        this: this_kind,
    })
}

/* Define an ad hoc function using a literal string inside the interpreter. */
fn snippet_text_closure(source: CodeSource, context: &[(&str, Value)], keys: &[&str], text: &str) -> Value {
    snippet_text_closure_abstract(source, ClosureThis::Never, context, keys, text)
}

fn snippet_text_method(source: CodeSource, context: &[(&str, Value)], keys: &[&str], text: &str) -> Value {
    snippet_text_closure_abstract(source, ClosureThis::Blank, context, keys, text)
}

fn snippet_apply(closure: &Value, value: Value) -> Value {
    match *closure {
        Value::Closure(cv @ ClosureValue {bound, need_args, ..})
        if need_args > 1 => {
            let b = bound.clone();
            b.push(value);
            
            Value::Closure(ClosureValue {
                bound: b,
                need_args: need_args - 1,
                ..cv
            })
        }
        _ => unreachable!()
    }
}

/* These first three snippet closures are relied on by the later ones */
lazy_static! {
    /* Ternary function without short-circuiting... */
    /* internal.tern exposes this */
    pub static ref RAW_TERN: Value = snippet_closure(3, |args|
        match *args {
            [Value::Null, _, v] => Ok(v),
            [_, v, _] => Ok(v),
            _ => impossible_arg("RAW_TERN")
        }
    );
    
    /* ...used to define the ternary function with short-circuiting: */
    /* This is used by snippets that require tern, but tern in scope_prototype is separate. */
    pub static ref TERN: Value = snippet_text_closure(
        CodeSource::Internal("TERN"),
        &[
            ("rawTern", *RAW_TERN),
            ("null", Value::Null)
        ],
        &["pred", "a", "b"],
        "(rawTern pred a b) null"
    );
}

/* This handles what occurs when you assign to a table while defining a new object literal.
   It takes newborn functions and assigns a this to them. (Old functions just freeze.) */
fn raw_rethis_assign_object_definition(obj: &Value, mut v: Value) -> Value {
    match v {
        Value::Closure(c @ ClosureValue {this: ClosureThis::Blank, ..}) =>
            c.this = ClosureThis::Current(box obj.clone(), box obj.clone()),
        
        Value::Closure(c @ ClosureValue {this: ClosureThis::Current(current, this), ..}) =>
            c.this = ClosureThis::Frozen(current, this),
        
        _ => {}
    }
    
    v    
}

/* This handles what occurs when you assign to a table at any other time:
   The "newborn" quality that makes it possible to assign a `this` is lost. */
fn raw_rethis_assign_object(mut v: Value) -> Value {
    if let Value::Closure(c @ ClosureValue {ref mut this, ..}) = v {
        match *this {
            ClosureThis::Blank =>
                c.this = ClosureThis::Never,
            ClosureThis::Current(current, this) =>
                c.this = ClosureThis::Frozen(current, this),
            _ => {}
        }
    }
    
    v
}

lazy_static! {
    /* Emily versions of the above two */
    pub static ref RETHIS_ASSIGN_OBJECT_DEFINITION: Value = snippet_closure(2, |args|
        match *args {
            [obj, a] => Ok(raw_rethis_assign_object_definition(&obj, a)),
            _ => impossible_arg("RETHIS_ASSIGN_OBJECT_DEFINITION")
        }
    );

    pub static ref RETHIS_ASSIGN_OBJECT: Value = snippet_closure(1, |args|
        match *args {
            [a] => Ok(raw_rethis_assign_object(a)),
            _ => impossible_arg("RETHIS_ASSIGN_OBJECT"),
        }
    );
}

/* This next batch is the functions required to create a blank user table */

/* Setup for filter-based functions */
/* FIXME: Remove need for target */
fn act_table_set(target: &Value, t: &mut TableValue, key: Value, value: Value) {
    t.insert(key, value);
    
    if options::RUN.read().unwrap().trace_set {
        println!("Set update {}", pretty::dump_value_new_table(target));
    }
}

fn act_table_set_with<F>(modifier: F, target: &Value, t: &mut TableValue, key: Value, value: Value) where
F: Fn(&Value, Value) -> Value {
    act_table_set(target, t, key, modifier(target, value));
}

fn act_pair_table_set(t1v: &Value, t1: &mut TableValue, t2v: &Value, t2: &mut TableValue, key: Value, value: Value) {
    act_table_set(t1v, t1, key, value);
    act_table_set(t2v, t2, key, value);
}

lazy_static! {
    /* Most tables need to be prepopulated with a "has". Here's the has tester for a singular table: */
    static ref RAW_HAS: Value = snippet_closure(2, |args|
        match *args {
            [Value::Table(ref t), key] |
            [Value::Object(ref t), key] =>
                Ok(t.contains_key(&key).into()),
            
            [v, _] => Err(bad_arg_table("RAW_HAS", &v)),
            _ => impossible_arg("RAW_HAS")
        }
    );
    
    /* A curried one which knows how to check the super class: */
    static ref HAS_CONSTRUCT: Value = snippet_text_closure(
        CodeSource::Internal("HAS_CONSTRUCT"),
        &[
            ("rawHas", *RAW_HAS),
            ("tern", *TERN),
            ("true", Value::True),
            ("null", Value::Null)
        ],
        &["obj", "key"],
        "tern (rawHas obj key) ^(true) ^(
             tern (rawHas obj .parent) ^(obj.parent.has key) ^(null)\
         )"
    );
}

/* ...And a factory for one with a preset object: */
fn make_has(obj: Value) -> Value {
    snippet_apply(&HAS_CONSTRUCT, obj)
}

lazy_static! {
    /* Most tables need to be prepopulated with a "set". Here's the setter for a singular table: */
    static ref RAW_SET: Value = snippet_closure(3, |args|
        match *args { /* TODO: Unify with make_let? */
            [tv @ Value::Table(t), key, value] |
            [tv @ Value::Object(t), key, value] => {
                act_table_set(&tv, &mut t, key, value);
                Ok(Value::Null)
            }
        
            [v, _, _] => Err(bad_arg_table("RAW_SET", &v)),
            _ => impossible_arg("RAW_SET")
        }
    );

    /* ...And a factory for a curried one that knows how to check the super class: */
    static ref SET_CONSTRUCT: Value = snippet_text_closure(
        CodeSource::Internal("SET_CONSTRUCT"),
        &[
            ("rawHas", *RAW_HAS),
            ("rawSet", *RAW_SET),
            ("tern", *TERN),
            ("true", Value::True),
            ("null", Value::Null)
        ],
        &["obj", "key", "value"],
        "tern (rawHas obj key) ^(rawSet obj key value) ^(
             obj.parent.set key value                # Note: Fails in an inelegant way if no parent\
         )"
    );
}

fn make_set(obj: Value) -> Value {
    snippet_apply(&SET_CONSTRUCT, obj)
}

lazy_static! {
    /* Same thing, but for a Value::Object instead of a Value::Table.
        The difference lies in how "this" is treated */
    static ref OBJECT_SET_CONSTRUCT: Value = snippet_text_closure(
        CodeSource::Internal("OBJECT_SET_CONSTRUCT"),
        &[
            ("rawHas", *RAW_HAS),
            ("rawSet", *RAW_SET),
            ("tern", *TERN),
            ("true", Value::True),
            ("null", Value::Null),
            ("modifier", *RETHIS_ASSIGN_OBJECT)
        ],
        &["obj", "key", "value"],
        "tern (rawHas obj key) ^(rawSet obj key (modifier value)) ^(
             obj.parent.set key (modifier value) # Note: Fails in an inelegant way if no parent\
         )"
    );
}

fn make_object_set(obj: Value) -> Value {
    snippet_apply(&OBJECT_SET_CONSTRUCT, obj)
}

/* Many tables need to be prepopulated with a "let". Here's the let setter for a singular table: */
/* TODO: Don't 'make' like this? */
// FIXME can't coerce this closure to fn pointer due to `action` being captured.
fn make_let<F: Fn(Value, Value)>(action: F) -> Value {
    snippet_closure(2, |args| match *args {
        [key, value] => {
            action(key, value);
            Ok(Value::Null)
        }
        _ => impossible_arg("make_let"),
    })
}

/* Helpers for table_blank */
fn populate_with_has(t: &mut TableValue) {
    /* FIXME: Should this ever be Value::Object...? */
    t.insert(HAS_KEY.clone(), make_has(Value::Table(t.clone())));
}

fn populate_with_set(t: &mut TableValue) {
    populate_with_has(t);
    t.insert(SET_KEY.clone(), make_set(Value::Table(t.clone())));
}

/* Not unified with table_blank because it returns a value */
pub fn object_blank(context: ExecuteContext) -> Value {
    let mut obj = table_true_blank();
    let obj_value = Value::Object(obj.clone());
    
    populate_with_has(&mut obj);
    obj.insert(
        SET_KEY.clone(),
        make_object_set(obj_value.clone())
    );
    obj.insert(
        LET_KEY.clone(),
        make_let(|k,v| act_table_set_with(|_, x| raw_rethis_assign_object(x), &obj_value, &mut obj, k, v))
    );
    obj.insert(
        PARENT_KEY.clone(),
        context.object_proto
    );
    obj_value
}

/* FIXME: Once act_table_set no longer takes a table value, the dummy Value::Table will not be needed */
// `table_blank` (note from present me: thanks past me, very helpful)
fn populate_let_for_scope(store_in: &mut TableValue, write_to: TableValue) {
    store_in.insert(
        LET_KEY.clone(),
        make_let(
            |k, v| act_table_set(&Value::Table(write_to.clone()), &mut write_to, k, v)
        )
    );
}

/* Give me a simple table of the requested type, prepopulate with basics. */
pub fn table_blank(kind: TableBlankKind) -> TableValue {
    let mut t = table_true_blank();
    match kind {
        TableBlankKind::TrueBlank => {}
        TableBlankKind::NoSet => populate_with_has(&mut t),
        TableBlankKind::NoLet => populate_with_set(&mut t),
        TableBlankKind::WithLet => {
            populate_with_set(&mut t);
            t.insert(LET_KEY.clone(), make_let(|k, v| {
                t.insert(k, v);
                
                if options::RUN.read().unwrap().trace_set {
                    println!("Set update {}", pretty::dump_value_new_table(&Value::Table(t)));
                }
            }));
        }
    }
    t
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BoxTarget { Package, Object }

pub enum BoxSpec { Populating(BoxTarget, Value) }

pub fn box_blank(box_kind: BoxSpec, box_parent: Value) -> TableValue {
    let BoxSpec::Populating(target_type, target_value) = box_kind;
    let mut t = table_blank(TableBlankKind::NoLet);
    let mut private_table = table_blank(TableBlankKind::NoLet);
    let private_value = Value::Table(private_table);
    let target_table = TableValue::from(target_value);
    
    private_table.insert(LET_KEY.clone(), make_let( /* Another fallacious value usage */
        |k, v| act_pair_table_set(&private_value, &mut private_table, &Value::Table(t), &mut t, k, v)
    ));
    
    t.insert(LET_KEY.clone(), make_let(|k, v| /* See objects.md */
        if target_type == BoxTarget::Package {
            act_pair_table_set(&target_value, &mut target_table, &Value::Table(t), &mut t, k, v)
        }
        else {
            act_table_set_with(raw_rethis_assign_object_definition, &target_value, &mut target_table, k, v)
        }
    ));
    
    if target_type == BoxTarget::Package {
        t.insert(EXPORT_LET_KEY.clone(), make_let(|k, v| act_table_set(&target_value, &mut target_table, k, v)));
    }
    
    t.insert(THIS_KEY.clone(), target_value);
    t.insert(PARENT_KEY.clone(), box_parent);
    t.insert(CURRENT_KEY.clone(), target_value);
    /* Access to a private value: */
    t.insert(PRIVATE_KEY.clone(), private_value);
    t
}

pub fn table_inheriting(table_kind: TableBlankKind, v: Value) -> TableValue {
    let mut t = table_blank(table_kind);
    t.insert(PARENT_KEY.clone(), v);
    t
}

/* Not used by interpreter, but present for user */
/*
fn raw_rethis_transplant(obj: Value) -> Value {
    match obj {
        Value::Closure(c) =>
            Value::Closure(ClosureValue {this: ClosureThis::Blank, ..c}),
        _ => obj,
    }
}
*/

lazy_static! {
    pub static ref RETHIS_TRANSPLANT: Value = snippet_closure(1, |args|
        match *args {
            //[obj] => raw_rethis_transplant(obj),
            [Value::Closure(c)] =>
                Ok(Value::Closure(ClosureValue {this: ClosureThis::Blank, ..c})),
            [obj] => Ok(obj),
            _ => impossible_arg("RETHIS_TRANSPLANT")
        }
    );
}

/* Helpers for super function */
/*
pub fn raw_rethis_super_from(obj: Value, v: Value) -> Value {
    match v {
        Value::Closure(c @ ClosureValue {this: ClosureThis::Current(current, _), ..}) =>
            Value::Closure(ClosureValue {
                this: ClosureThis::Current(current, box obj),
                ..c
            }),
        _ => v,
    }
}
*/

lazy_static! {
    pub static ref RETHIS_SUPER_FROM: Value = snippet_closure(2, |args|
        match *args {
            //[obj, a] => raw_rethis_super_from(obj, a),
            [obj, Value::Closure(c @ ClosureValue {this: ClosureThis::Current(current, _), ..})] =>
                Ok(Value::Closure(ClosureValue {
                    this: ClosureThis::Current(current, box obj),
                    ..c
                })),
            [_, a] => Ok(a),
            _ => impossible_arg("RETHIS_SUPER_FROM")
        }
    );

    static ref MISAPPLY_ARG: Value = snippet_closure(2, |args|
        match *args {
            [a, b] => Err(misapply_string(&a, &b)),
            _ => impossible_arg("MISAPPLY_ARG")
        }
    );

    /* Factory for super functions */
    static ref SUPER_CONSTRUCT: Value = snippet_text_closure(
        CodeSource::Internal("SUPER_CONSTRUCT"),
        &[
            ("rethis", *RETHIS_SUPER_FROM),
            ("rawHas", *RAW_HAS),
            ("tern", *TERN),
            ("misapplyArg", *MISAPPLY_ARG)
        ],
        &["callCurrent", "obj", "arg"],
        "tern (rawHas callCurrent .parent) ^(rethis obj (callCurrent.parent arg)) ^(misapplyArg obj arg)"
    );
}

pub fn make_super(current: Value, this: Value) -> Value {
    snippet_apply(&snippet_apply(&*SUPER_CONSTRUCT, current), this)
}

pub fn stack_string(stack: &ExecuteStack) -> String {
    let mut result = "Stack:".to_owned();
    
    for frame in stack.iter().rev() {
        result += "\n\t";
        
        match frame {
            ExecuteFrame {register: RegisterState::LineStart(..), code, ..}
            if !code.is_empty() && !code[0].is_empty() =>
                result += &code[0][0].at.to_string(),
            ExecuteFrame {register: RegisterState::FirstValue(_, _, at), ..} |
            ExecuteFrame {register: RegisterState::PairValue(_, _, _, at), ..} =>
                result += &at.to_string(),
            ExecuteFrame {code, ..} if code.is_empty() =>
                result += "<empty file>",
            ExecuteFrame {code, ..} if code[0].is_empty() =>
                result += "<lost place>",
            _ => unreachable!()
        }
    }
    
    result
}

pub fn raw_misapply_stack(stack: ExecuteStack, a: &Value, b: &Value) -> Result<(), String> {
    Err(format!("{}\n{}", misapply_string(a, b), stack_string(&stack)))
}

/*
fn make_lazy<F>(table: &mut TableValue, key: Value, func: F) -> Value where
F: Fn() -> Value {
    Value::BuiltinUnaryMethod(move |_| { /* Later maybe pass on this to func? */
        let result = func();
        table.insert(key, result);
        result
    })
}

pub fn table_set_lazy<F>(table: &mut TableValue, key: Value, func: F) where
F: Fn() -> Value {
    table.insert(key, make_lazy(table, key, func));
}
*/
