/* This file contains support methods for creating values with certain properties, split out from Value for module recursion reasons. */
#![feature(slice_patterns, advanced_slice_patterns)]

#[macro_use]
extern crate lazy_static;

use std::collections::HashMap;
use value::*;
use token::*;

/* Misc failure throw methods */
pub fn bad_arg(desired: &'static str, name: &'static str, var: &Value) -> Result<(), String> {
	Err (format!("Bad argument to {}: Need {}, got {}", name, desired, pretty::dump_value(var)))
}

pub fn bad_arg_table(name: &'static str, var: &Value) -> Result<(), String> {
	bad_arg("table", name, var)
}

pub fn bad_arg_closure(name: &'static str, var: &Value) -> Result<(), String> {
	bad_arg("closure", name, var)
}

pub fn impossible_arg(name: &'static str) -> Result<(), String> {
	Err (format!("Internal failure: Impossible argument to {}", name))
}


pub fn misapply_string(a: &Value, b: &Value) -> String {
	format!("Runtime failure: {} can't respond to {}", pretty::dump_value(a), pretty::dump_value(b))
}

pub fn raw_misapply_arg(a: &Value, b: &Value) -> Result<(), String> {
	Err (misapply_string(a, b))
}

/* Tools */
pub fn bool_cast(v: bool) -> Value {
	if v { Value::True }
	else { Value::Null }
}

/* Create a closure from a function */

pub fn snippet_closure<F: Fn(Vec<Value>) -> Value>(arg_count: usize, exec: F) -> Value {
	Value::Closure (ClosureValue {
		exec: ClosureExec::Builtin (exec),
		need_args: arg_count,
		bound: vec![],
		this: ClosureThis::Never
	})
}

/* For debugging, call this after creating a hashtable set to become a Value */
pub fn seal_table(t: &mut TableValue) {
	if options::RUN.track_objects {
		unsafe {
			ID_GENERATOR += 1.0;
			t.insert(value::ID_KEY.clone(), Value::Float (ID_GENERATOR));
		}
	}
}

/* Same as calling table_blank(TrueBlank). We need a separate version because
   table_blank relies on some of the functions that require table_true_blank
   to themselves be defined, and it gets awkward w/mutual recursion. */
pub fn table_true_blank() -> TableValue {
	let mut t = HashMap::new();
	seal_table(&mut t);
	t
}

pub fn table_true_blank_inheriting(v: &Value) -> TableValue {
	let mut t = table_true_blank();
	t.insert(value::PARENT_KEY.clone(), v.clone());
	t
}

/* Makes a scope to be used in a snippet_text_closure */
pub fn snippet_scope(bindings: Vec<(String, Value)>) -> Value {
	let mut scope_table = table_true_blank();
	for (k, v) in bindings {
		scope_table.insert(Value::Atom (k), v);
	}
	Value::Table (scope_table)
}

/* Define an ad hoc function using a literal string inside the interpreter. */
pub fn snippet_text_closure_abstract(source: CodeSource, this_kind: ClosureThis, context: Vec<(String, Value)>, keys: Vec<String>, text: &'static str) -> Value {
	Value::Closure (ClosureValue {
		exec: ClosureExec::User {
			body: tokenize::snippet(source.clone(), text.to_string()),
			env_scope: snippet_scope(context),
			scoped: false,
			key: keys,
			has_return: false,
		},
		need_args: keys.len(),
		bound: vec![],
		this_value: this_kind
	})
}

/* Define an ad hoc function using a literal string inside the interpreter. */
pub fn snippet_text_closure(source: CodeSource, context: Vec<(String, Value)>, keys: Vec<String>, text: &'static str) -> Value {
	snippet_text_closure_abstract(source, ClosureThis::Never, context, keys, text)
}

pub fn snippet_text_method(source: CodeSource, context: Vec<(String, Value)>, keys: Vec<String>, text: &'static str) -> Value {
	snippet_text_closure_abstract(source, ClosureThis::Blank, context, keys, text)
}

pub fn snippet_apply(closure: &Value, val: Value) -> ClosureValue {
	match *closure {
		Value::Closure (cv @ ClosureValue {bound, need_args, ..}) if need_args > 1 => ClosureValue {
			bound: bound.iter().cloned().chained(vec![val]).collect(),
			need_args: need_args - 1,
			.. cv
		},
		_ => Err("Internal error"),
	}
}

/* These first three snippet closures are relied on by the later ones */
lazy_static! {
	/* Ternary function without short-circuiting... */
	/* internal.tern exposes this */
	pub static ref RAW_TERN: Value = snippet_closure(3, |args| match &*args {
		[Value::Null, _, v] => v,
		[_, v, _] => v,
		_ => impossible_arg("RAW_TERN"),
	});
	
	/* ...used to define the ternary function with short-circuiting: */
	/* This is used by snippets that require tern, but tern in scope_prototype is separate. */
	pub static ref TERN: Value = snippet_text_closure(
		CodeSource::Internal ("TERN"),
		vec![
			("rawTern", RAW_TERN),
			("null", Value::Null)
		],
		vec!["pred", "a", "b"],
		"(rawTern pred a b) null"
	);
}

/* This handles what occurs when you assign to a table while defining a new object literal.
   It takes newborn functions and assigns a this to them. (Old functions just freeze.) */
pub fn raw_rethis_assign_object_definition(obj: &Value, mut v: Value) -> Value {
	match v {
		Value::Closure (c @ ClosureValue {this: ClosureThis::Blank, ..}) =>
			c.this = ClosureThis::Current(obj, obj.clone()),
		
		Value::Closure (c @ ClosureValue {this: ClosureThis::Current(current, this), ..}) =>
			c.this = ClosureThis::Frozen(current, this),
		
		_ => {}
	}
	
	v	
}

/* This handles what occurs when you assign to a table at any other time:
   The "newborn" quality that makes it possible to assign a `this` is lost. */
pub fn raw_rethis_assign_object(mut v: Value) -> Value {
	match v {
		Value::Closure (c @ ClosureValue {this: ClosureThis::Blank, ..}) =>
			c.this = ClosureThis::Never,
		
		Value::Closure (c @ ClosureValue {this: ClosureThis::Current(current, this), ..}) =>
			c.this = CurrentThis::Frozen(current, this),
		
		_ => {}
	}
	
	v
}

lazy_static! {
	/* Emily versions of the above two */
	pub static ref RETHIS_ASSIGN_OBJECT_DEFINITION: Value = snippet_closure(2, |args|
		match &*args {
			[obj, a] => raw_rethis_assign_object_definition(obj, a),
			_ => impossible_arg("RETHIS_ASSIGN_OBJECT_DEFINITION")
		}
	);

	pub static ref RETHIS_ASSIGN_OBJECT: Value = snippet_closure(1, |args|
		match &*args {
			[a] => raw_rethis_assign_object(a),
			_ => impossible_arg("RETHIS_ASSIGN_OBJECT");
		}
	);
}

/* This could have been done in-place with a k combinator */
pub fn rethis_assign_object_inside_let(_: Value, x: Value) -> Value {
	raw_rethis_assign_object(x)
}

/* This next batch is the functions required to create a blank user table */

/* Setup for filter-based functions */
/* FIXME: Remove need for target */
pub fn act_table_set(target: &Value, t: &mut TableValue, key: Value, value: Value) {
	t.insert(key, value);
	
	if options::RUN.trace_set {
		println!("Set update {}", pretty::dump_value_new_table(target));
	}
}

pub fn act_table_set_with(modifier: F, target: Value, t: TableValue, key: Value, value: Value) where
F: Fn(Value, Value) -> Value {
	act_table_set(target, t, key, modifier(target, value));
}

pub fn act_pair_table_set(t1v: Value, t1: TableValue, t2v: Value, t2: TableValue, key: Value, value: Value) {
	act_table_set(t1v, t1, key, value);
	act_table_set(t2v, t2, key, value);
}

lazy_static! {
	/* Most tables need to be prepopulated with a "has". Here's the has tester for a singular table: */
	pub static ref RAW_HAS: Value = snippet_closure(2, |args| match &*args {
		[Value::Table (t), key] | [Value::Object (t), key] =>
			bool_cast(t.contains_key(key)),
		
		[v, _] => bad_arg_table("RAW_HAS", v),
		_ => impossible_arg("RAW_HAS")
	});

	/* A curried one which knows how to check the super class: */
	pub static ref HAS_CONSTRUCT: Value = snippet_text_closure(
		CodeSource::Internal ("HAS_CONSTRUCT"),
		vec![
			("rawHas", RAW_HAS),
			("tern", TERN),
			("true", Value::True),
			("null", Value::Null)
		],
		vec!["obj", "key"],
		"tern (rawHas obj key) ^(true) ^(
		     tern (rawHas obj .parent) ^(obj.parent.has key) ^(null)\
		 )"
	);
}

/* ...And a factory for one with a preset object: */
pub fn make_has(obj: Value) -> ClosureValue {
	snippet_apply(HAS_CONSTRUCT, obj)
}

lazy_static! {
	/* Most tables need to be prepopulated with a "set". Here's the setter for a singular table: */
	pub static ref RAW_SET: Value = snippet_closure(3, |args| match &*args { /* TODO: Unify with make_let? */
		[tv @ Value::Table (t), key, value] | [tv @ Value::Object (t), key, value] => {
			act_table_set(tv, t, key, value);
			Value::Null
		}
		
		[v, _, _] => bad_arg_table("RAW_SET", v),
		_ => impossible_arg("RAW_SET")
	});

	/* ...And a factory for a curried one that knows how to check the super class: */
	pub static ref SET_CONSTRUCT: Value = snippet_text_closure(
		CodeSource::Internal ("SET_CONSTRUCT"),
		vec![
			("rawHas", RAW_HAS),
			("rawSet", RAW_SET),
			("tern", TERN),
			("true", Value::True),
			("null", Value::Null)
		],
		vec!["obj", "key", "value"],
		"tern (rawHas obj key) ^(rawSet obj key value) ^(
		     obj.parent.set key value                # Note: Fails in an inelegant way if no parent\
		 )"
	);
}

pub fn make_set(obj: Value) -> ClosureValue {
	snippet_apply(SET_CONSTRUCT, obj)
}

lazy_static! {
/* Same thing, but for a Value::Object instead of a Value::Table.
   The difference lies in how "this" is treated */
	pub static ref OBJECT_SET_CONSTRUCT: Value = snippet_text_closure(
		CodeSource::Internal ("OBJECT_SET_CONSTRUCT"),
		vec![
			("rawHas", RAW_HAS),
			("rawSet", RAW_SET),
			("tern", TERN),
			("true", Value::True),
			("null", Value::Null),
			("modifier", RETHIS_ASSIGN_OBJECT)
		],
		vec!["obj", "key", "value"],
		"tern (rawHas obj key) ^(rawSet obj key (modifier value)) ^(
		     obj.parent.set key (modifier value) # Note: Fails in an inelegant way if no parent\
		 )"
	);
}

pub fn make_object_set(obj: Value) -> ClosureValue {
	snippet_apply(OBJECT_SET_CONSTRUCT, obj)
}

/* Many tables need to be prepopulated with a "let". Here's the let setter for a singular table: */
/* TODO: Don't 'make' like this? */
pub fn make_let<F: Fn(Value, Value)>(action: F) -> Value {
	snippet_closure(2, |args| match &*args {
		[key, value] => {
			action(key, value);
			Value::Null
		}
		_ => impossible_arg("make_let"),
	})
}

/* Helpers for table_blank */
pub fn populate_with_has(t: &mut TableValue) {
	/* FIXME: Should this ever be ObjectValue...? */
	t.insert(Value::Atom (HAS_KEY_STRING.to_string()), make_has(Value::Table (t.clone())));
}

pub fn populate_with_set(t: &mut TableValue) {
	populate_with_has(t);
	t.insert(Value::Atom (SET_KEY_STRING.to_string()), make_set(Value::Table (t.clone())));
}

/* Not unified with table_blank because it returns a value */
pub fn object_blank(context: ExecuteContext) -> Value {
	let mut obj = table_true_blank();
	let obj_value = Value::Object (obj.clone());
	
	populate_with_has(&mut obj);
	obj.insert(
		Value::Atom (SET_KEY_STRING.to_string()),
		Value::Closure (make_object_set(obj_value.clone()))
	);
	obj.insert(
		Value::Atom (LET_KEY_STRING.to_string()),
		make_let(act_table_set_with(
			rethis_assign_object_inside_let,
			obj_value,
			obj
		))
	);
	obj.insert(
		Value::Atom (PARENT_KEY_STRING.to_string()),
		context.object_proto
	);
	obj_value
}

/* FIXME: Once act_table_set no longer takes a table value, the dummy Value::Table will not be needed */
pub fn populate_let_for_scope(store_in: TableValue, write_to: TableValue) {
	store_in.insert(
		Value::Atom (LET_KEY_STRING.to_string()),
		make_let(
			act_table_set(
				Value::Table (write_to),
				write_to
			)
		)
	)
}

/* Give me a simple table of the requested type, prepopulate with basics. */
pub fn table_blank(kind: TableBlankKind) -> TableType {
	let t = table_true_blank();
	match kind {
		TableBlankKind::TrueBlank => {}
		TableBlankKind::NoSet => populate_with_has(t),
		TableBlankKind::NoLet => populate_with_set(t),
		TableBlankKind::WithLet => {
			populate_with_set(t);
			populate_let_for_scope(t, t);
		}
	}
	t
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BoxTarget { Package, Object }

pub enum BoxSpec { Populating(BoxTarget, Value) }

pub fn box_blank(box_kind: BoxSpec, box_parent: Value) -> TableType {
	let BoxSpec::Populating (target_type, target_value) = box_kind;
    let mut t = table_blank(TableBlankKind::NoLet);
    let mut private_table = table_blank(TableBlankKind::NoLet);
    let private_value = Value::Table (private_table);
    let target_table = table_from(target_value);
    
    private_table.insert(value::LET_KEY.clone(), make_let( /* Another fallacious value usage */
    	// TODO: currying
        act_pair_table_set(private_value, private_table, Value::Table (t), t)
    ));
    
    t.insert(Value::LET_KEY.clone(), make_let( /* See objects.md */
        if targetType == Package {
        	// TODO: currying
            act_pair_table_set(target_value, target_table, Value::Table (t), t)
        }
        else {
            act_table_set_with(raw_rethis_assign_object_definition, target_value, target_table)
        }
    ));
    
    if target_type == Package {
        t.insert(value::EXPORT_LET_KEY.clone(), make_let(act_table_set(target_value, target_table)))
    }
    
    t.insert(value::THIS_KEY.clone(), target_value);
    t.insert(value::PARENT_KEY.clone(), box_parent);
    t.insert(value::CURRENT_KEY.clone(), target_value);
    /* Access to a private value: */
    t.insert(value::PRIVATE_KEY.clone(), private_value);
    t
}

pub fn table_inheriting(table_kind: TableBlankKind, v: Value) -> TableType {
	let mut t = table_blank(table_kind);
	t.insert(value::PARENT_KEY.clone(), v);
	t
}

/* Not used by interpreter, but present for user */
pub fn raw_rethis_transplant(obj: Value) -> Value {
	match obj {
		Value::Closure (c) =>
			Value::Closure (ClosureValue {this: ClosureThis::Blank, ..c}),
		obj => obj,
	}
}

lazy_static! {
	pub static ref RETHIS_TRANSPLANT: Value = snippet_closure(1, |args|
		match &*args {
			[mut ref obj] => raw_rethis_transplant(obj),
			_ => impossible_arg("RETHIS_TRANSPLANT")
		}
	);
}

/* Helpers for super function */
pub fn raw_rethis_super_from(obj: Value, v: Value) -> Value {
	match v {
		Value::Closure (c @ ClosureValue {this: ClosureThis::Current (current, _), ..}) =>
			Value::Closure (ClosureValue {
				this: ClosureThis::Current (current, obj),
				..c
			}),
		v => v,
	}
}

lazy_static! {
	pub static ref RETHIS_SUPER_FROM: Value = snippet_closure(2, |args|
		match &*args {
			[obj, a] => raw_rethis_super_from(obj, a),
			_ => impossible_arg("RETHIS_SUPER_FROM")
		}
	);

	pub static ref MISAPPLY_ARG: Value = snippet_closure(2, |args|
		match &*args {
			[a, b] => raw_misapply_arg(a, b),
			_ => impossible_arg("MISAPPLY_ARG")
		}
	);

	/* Factory for super functions */
	pub static ref SUPER_CONSTRUCT: Value = snippet_text_closure(
		CodeSource::Internal ("SUPER_CONSTRUCT"),
		vec![
			("rethis", RETHIS_SUPER_FROM),
			("rawHas", RAW_HAS),
			("tern", TERN),
			("misapplyArg", MISAPPLY_ARG)
		],
		vec!["callCurrent", "obj", "arg"],
		"tern (rawHas callCurrent .parent) ^(rethis obj (callCurrent.parent arg)) ^(misapplyArg obj arg)"
	);
}

pub fn make_super(current: Value, this: Value) -> ClosureValue {
	snippet_apply(&Value::Closure (snippet_apply(SUPER_CONSTRUCT, current)), this)
}

pub fn stack_string(stack: &ExecuteStack) -> String {
	let mut result = "Stack:".to_string();
	for frame in stack.iter().rev() {
		result.push_str("\n\t");
		result.push_str(match *frame {
			ExecuteFrame {register: RegisterState::LineStart (..), ref code, ..}
			if !code.is_empty() && !code[0].is_empty() =>
				&code[0][0].at.to_string(),
			ExecuteFrame {register: RegisterState::FirstValue (_, _, ref at), ..} |
			ExecuteFrame {register: RegisterState::PairValue (_, _, _, ref at), ..} =>
				&at.to_string(),
			ExecuteFrame {ref code, ..}
			if code.is_empty() =>
				"<empty file>",
			ExecuteFrame {ref code, ..}
			if code[0].is_empty() =>
				"<lost place>",
		});
	}
	result
}

pub fn raw_misapply_stack(stack: ExecuteStack, a: &Value, b: &Value) -> Result<(), String> {
	Err(format!("{}\n{}", misapply_string(a, b), stack_string(&stack)))
}

pub fn make_lazy<F>(table: &mut TableValue, key: Value, func: F) -> Value where
F: Fn() -> Value {
	Value::BuiltinUnaryMethod (move |_| { /* Later maybe pass on this to func? */
		let result = func();
		table.insert(key, result);
		result
	})
}

pub fn table_set_lazy<F>(table: &mut TableValue, key: Value, func: F) where
F: Fn() -> Value {
	table.insert(key, make_lazy(table, key, func));
}
