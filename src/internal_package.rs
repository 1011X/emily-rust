#![feature(slice_patterns)]

use std::sync::{
	Once,
	ONCE_INIT
};

use value;
use value_util;

use value::{
	TableBlankKind,
	Value
};
use value_util::TableType;

pub fn table_pair() -> (TableType, Value) {
	let table = value_util::table_blank(TableBlankKind::NoSet);
	let value = Value::Table (table);
	(table, value)
}

/* Sign-of-divisor modulus */
pub fn modulus(a: f64, b: f64) -> f64 {
	(a % b + b) % b
}

lazy_static! {
	pub static ref TRUEFN_VALUE: Value = Value::BuiltinFunction (|x| Value::True);
	
	pub static ref INTERNAL_TABLE: TableType = value_util::table_blank(TableBlankKind::NoSet);
	pub static ref INTERNAL_VALUE: Value = Value::Table (INTERNAL_TABLE);
}

pub fn fake_register_location(name: &'static str) -> CodePosition {
	CodePosition {
		file_name: CodeSource::Internal (name),
		line_number: 0,
		line_offset: 0
	}
}

pub fn fake_register_from(reg: RegisterState) -> ExecuteFrame {
	ExecuteFrame {
		register: reg,
		scope: Value::Null,
		code: vec![]
	}
}

static START: Once = ONCE_INIT;

pub fn init() {
// may seem like we can remove START, but it ensures that the initialization
// only ever occurs once for this module when calling init().
START.call_once(|| {
	let set_atom_value = |table, name, v| table.unwrap_or(INTERNAL_TABLE).insert(Value::Atom(name), v);
	let set_atom_fn = |table, n, func| set_atom_value(table, n, Value::BuiltinFunction(func));
	/* let set_atom_handoff = |table, n, func| set_atom_value(table, n, Value::BuiltinHandoff (func));*/
	let set_atom_binary = |table, n, func| set_atom_value(table, n, value_util::snippet_closure(2, |x| match &*x {
		[a, b] => func(a, b),
		_ => value_util::impossible_arg("<builtin-pair>")
	}));
	let insert_table = |table, n| {
		let (sub_table, sub_value) = table_pair();
		set_atom_value(table, n, sub_value);
		sub_table
	};

	/* Create a function that consumes an argument, then returns itself. `func` should return void */
	// TODO
	fn reusable<F: Fn(Value)>(func: F) -> fn(Value) -> Value {
		fn inner(arg: Value) {
			func(arg);
			Value::BuiltinFunction (inner)
		}
		inner
	}

	set_atom_value(None, "tern", value_util::RAW_TERN);
	set_atom_value(None, "true", Value::True);

	set_atom_fn(None, "not", |v| match v { Value::Null => Value::True, _ => Value::Null });
	set_atom_binary(None, "primitiveEq", |a, b| value_util::bool_cast(a == b));

	set_atom_value(None, "thisTransplant", value_util::RETHIS_TRANSPLANT);
	set_atom_value(None, "thisInit", value_util::RETHIS_ASSIGN_OBJECT_DEFINITION);
	set_atom_value(None, "thisFreeze", value_util::RETHIS_ASSIGN_OBJECT);
	set_atom_value(None, "thisUpdate", value_util::RETHIS_SUPER_FROM);

	set_atom_value(None, "setPropertyKey", value_util::snippet_closure(3, |x| match &*x {
		[Value::Table (mut ref t), k, v] |
		[Value::Object (mut ref t), k, v] => {
			t.insert(k, Value::UserMethod (v));
			Value::Null
		}
		[_, _, _] => failwith "Attempted to call setPropertyKey on something other than an object",
		_ => unreachable!(),
	}));

	set_atom_value(None, "fail", Value::BuiltinHandoff (|_, stack, value| {
		let message = match value {
			(Value::String (s), _) => format!("Program failed: {}", s),
			(v, _) => format!("Program failed with value: {}", pretty::dump_value_for_user(v))
		};
		execute::fail_with_stack(stack, message)
	}));

	/* This has to be a handoff because that's the only way to get context, needed to allocate an object */
	/* All this really does is convert options::RUN.args into an Emily list */
	set_atom_value(None, "getArgs", Value::BuiltinHandoff (|context, stack, (_, at)| {
		let o = value_util::object_blank(context);
		let mut ot = value::table_from(o);
		let ref args = options::RUN.args;
		
		ot.insert(Value::Atom ("count".to_string()), Value::Float (args.len() as f64));
		
		for (i, st) in args.iter().enumerate() {
			ot.insert(Value::Float (i as f64), Value::String(st));
		}
		execute::return_to(context, stack, (o, at))
	}));

	/* "Submodule" internal.out */
	let out_table = insert_table(None, "out");
	set_atom_fn(Some (out_table), "print", reusable(|v| print_string(pretty::dump_value_for_user(v))));
	set_atom_fn(Some (out_table), "flush", reusable(|_| flush_all () ));

	/* "Submodule" internal.double */
	let double_table = insert_table(None, "double");

	let set_atom_math = |table, name, f| set_atom_value(Some (table.unwrap_or(double_table)), name, value_util::snippet_closure(2, |x| match &*x {
		[Value::Float (f1), Value::Float (f2)] => Value::Float (f(f1, f2)),
		[Value::Float (_), _] => failwith "Don't know how to combine that with a number",
		_ => unreachable!(),
	}));

	let set_atom_test = |table, name, f| set_atom_value(Some (table.unwrap_or(double_table)), name, value_util::snippet_closure(2, |x| match &*x {
		[Value::Float (f1), Value::Float (f2)] => value_util::bool_cast(f(f1, f2)),
		[Value::Float (_), _] => failwith "Don't know how to compare that to a number",
		_ => unreachable!(),
	}));

	let set_atom_math_fn = |table, name, f| set_atom_fn(Some (table.unwrap_or(double_table)), name, |x| match x {
		Value::Float (f1) => Value::Float (f(f1)),
		_ => failwith "Can only perform that function on a number"
	});

	set_atom_math(None, "add", |a, b| a + b);
	set_atom_math(None, "subtract", |a, b| a - b);
	set_atom_math(None, "multiply", |a, b| a * b);
	set_atom_math(None, "divide", |a, b| a / b);
	set_atom_math(None, "modulus", modulus);

	/* Do I really need all four comparators? */
	set_atom_test(None, "lessThan", |a, b| a < b);
	set_atom_test(None, "lessThanEqual", |a, b| a <= b);
	set_atom_test(None, "greaterThan", |a, b| a > b);
	set_atom_test(None, "greaterThanEqual", |a, b| a >= b);

	set_atom_math_fn(None, "floor", f64::floor);

	set_atom_fn(Some (double_table), "toString", |x| match x {
		Value::Float (f1) => Value::String (f1.to_string()),
		_ => failwith "Can only perform that function on a number"
	});

	/* "Submodule" internal.string */
	let atom_table = insert_table(None, "atom");

	set_atom_fn(Some (atom_table), "toString", |x| match x {
		Value::Atom (s) => Value::String (s),
		_ => failwith "Can only perform that function on an atom"
	});

	/* "Submodule" internal.string */
	let string_table = insert_table(None, "string");

	/* Note: Does NOT coerce into a type, f is of type f -> value */
	let set_atom_string_op = |table, name, f| set_atom_value(Some (table.unwrap_or(string_table)), name, value_util::snippet_closure(1, |x| match &*x {
		[Value::String (f1)] => f(f1),
		_ => failwith "Can only perform that operation on a string"
	}));

	let uchar_to_codepoint = |u| Value::Float (u as f64);
	let uchar_to_string = |u| {
		let buffer = String::new();
		let enc = Uutf.encoder(`UTF_8, `Buffer (buffer));
		Uutf.encode(enc, `Uchar (u));
		Uutf.encode(enc, `End);
		Value::String (buffer)
	};
	let iterator_value = |filter, st| {
		let loc = fake_register_location("internal.string.iterUtf8");
		let decoder = Uutf.decoder(~encoding:`UTF_8, `String(st));
		Value::BuiltinHandoff (|context, stack, (f, at)| match Uutf.decode(decoder) {
			`Uchar (u) => {
				let result = filter(u);
				let mut v = stack.clone();
				v.push(fake_register_from(Value::First (TRUEFN_VALUE, loc, loc));
				v.push(fake_register_from(Value::Pair (f, result, loc, loc)));
				execute::execute_step(context, v)
			}
			_ => execute::return_to(context, stack, (Value::Null, loc))
		})
	};
	set_atom_string_op("iterUtf8", iterator_value(uchar_to_string));
	set_atom_string_op("iterUtf8Codepoint", iterator_value(uchar_to_codepoint));

	set_atom_value(Some (string_table), "codepointToString", value_util::snippet_closure(1, |x| match &*x {
		[Value::Float (u)] => uchar_to_string(u as i32),
		_ => failwith "Can only perform that operation on a number"
	}));

	set_atom_value(Some (string_table), "concat", value_util::snippet_closure(2, |x| match &*x {
		[Value::String (f1), Value::String (f2)] => Value::String (format!("{}{}", f1, f2)),
		[Value::String (_), _] => failwith "Don't know how to combine that with a string",
		_ => unreachable!(),
	}));

	/* "Submodule" internal.type */
	let type_table = insert_table(None, "type");

	set_atom_fn(Some (type_table), "isAtom", |v| match v { Value::Atom (_) => Value::True, _ => Value::Null});
	set_atom_fn(Some (type_table), "isString", |v| match v { Value::String (_) => Value::True, _ => Value::Null});
	set_atom_fn(Some (type_table), "isNumber", |v| match v { Value::Float (_) => Value::True, _ => Value::Null});

	/* "Submodule" internal.type */
	
	if cfg!(BUILD_INCLUDE_C_FFI) {
		use ffi_support::*;
		let ffi_table = insert_table(None, "ffi");
		
		set_atom_fn(Some (ffi_table), "newForeign", |_| {
			let foreigner = {name=None; args=[]; returning="void"};
			let table = value_util::table_blank(TableBlankKind::NoSet);
			let set_ffi_param = |what, func| table.insert(Value::Atom (what), Value::BuiltinFunction (|a| match a {
				Value::Atom (s) |
				Value::String (s) => { func(s); Value::Null }
				x => failwith @@ format!("Need key {} for ffi {}; expected string or atom", x, what),
			}));
			set_ffi_param("name", |s| foreigner.name = Some (s));
			set_ffi_param("return", |s| foreigner.returning = s);
			set_ffi_param("args", |s| foreigner.args = {
				let t = foreigner.args.clone();
				t.push(s);
				t
			});
			table.insert(Value::Atom ("make".to_string()), Value::BuiltinFunction (|_|
				match foreigner.name {
					None => failwith "No name provided for FFI function",
					Some (name) =>
						value_foreign(name, foreigner.args.clone().reverse(), foreigner.returning)
				}
			));
			Value::Table (table)
		});
	}

	/* Done */
});
}
