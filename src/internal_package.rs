use std::sync::{Once, ONCE_INIT};
use std::borrow::Cow;

use ocaml;
use execute;
use value_util;
use pretty;
use options;
use value::{
    self,
    ExecuteFrame,
    RegisterState,
    TableValue,
    TableBlankKind,
    Value,
};
use token::{
    CodeSource,
    CodePosition,
};

pub fn table_pair() -> (TableValue, Value) {
    let table = value_util::table_blank(TableBlankKind::NoSet);
    let value = Value::Table (table);
    (table, value)
}

lazy_static! {
    pub static ref TRUEFN_VALUE: Value = Value::BuiltinFunction(box |_| Value::True);
    
    pub static ref INTERNAL_TABLE: TableValue = value_util::table_blank(TableBlankKind::NoSet);
    pub static ref INTERNAL_VALUE: Value = Value::Table(INTERNAL_TABLE);
}

pub fn fake_register_location(name: &'static str) -> CodePosition {
    CodePosition {
        file_name: CodeSource::Internal(name),
        line_number: 0,
        line_offset: 0,
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
    let set_atom_value = |table, name, v|
        table.unwrap_or(INTERNAL_TABLE).insert(Value::Atom(Cow::from(name)), v);
    
    let set_atom_fn = |table, n, func|
        set_atom_value(table, n, Value::BuiltinFunction(func));
    
    /* let set_atom_handoff = |table, n, func|
        set_atom_value(table, n, Value::BuiltinHandoff(func));*/
    
    let set_atom_binary = |table, n, func|
        set_atom_value(table, n, value_util::snippet_closure(2, box |x| match *x {
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
    /*
    fn reusable<F: Fn(Value)>(func: F) -> fn(Value) -> Value {
        fn inner(arg: Value) {
            func(arg);
            Value::BuiltinFunction(inner)
        }
        inner
    }
    */

    set_atom_value(None, "tern", value_util::RAW_TERN);
    set_atom_value(None, "true", Value::True);

    set_atom_fn(None, "not", box |v| match v { Value::Null => Value::True, _ => Value::Null });
    set_atom_binary(None, "primitiveEq", box |a, b| value_util::bool_cast(a == b));

    set_atom_value(None, "thisTransplant", value_util::RETHIS_TRANSPLANT);
    set_atom_value(None, "thisInit", value_util::RETHIS_ASSIGN_OBJECT_DEFINITION);
    set_atom_value(None, "thisFreeze", value_util::RETHIS_ASSIGN_OBJECT);
    set_atom_value(None, "thisUpdate", value_util::RETHIS_SUPER_FROM);

    set_atom_value(None, "setPropertyKey", value_util::snippet_closure(3, |x| match *x {
        [Value::Table(ref mut t), k, v]
        | [Value::Object(ref mut t), k, v] => {
            t.insert(k, Value::UserMethod(v));
            Value::Null
        }
        [_, _, _] => ocaml::failwith("Attempted to call setPropertyKey on something other than an object"),
        _ => unreachable!()
    }));

    set_atom_value(None, "fail", Value::BuiltinHandoff(|_, stack, value| {
        let message = match value {
            (Value::String(ref s), _) => format!("Program failed: {}", s),
            (v, _) => format!("Program failed with value: {}", pretty::dump_value_for_user(v))
        };
        execute::fail_with_stack(stack, message)
    }));

    /* This has to be a handoff because that's the only way to get context, needed to allocate an object */
    /* All this really does is convert options::RUN.args into an Emily list */
    set_atom_value(None, "getArgs", Value::BuiltinHandoff(|context, stack, (_, at)| {
        let o = value_util::object_blank(context);
        let mut ot = value::table_from(o);
        let ref args = options::RUN.args;
        
        ot.insert(Value::Atom(Cow::from("count")), Value::Float(args.len() as f64));
        
        for (i, st) in args.iter().enumerate() {
            ot.insert(Value::Float(i as f64), Value::String(st));
        }
        
        execute::return_to(context, stack, (o, at))
    }));

    /* "Submodule" internal.out */
    let out_table = insert_table(None, "out");
    //set_atom_fn(Some(out_table), "print", reusable(|v| print_string(pretty::dump_value_for_user(v))));
    //set_atom_fn(Some(out_table), "flush", reusable(|_| /*// flush_all */ () ));

    /* "Submodule" internal.double */
    let double_table = insert_table(None, "double");

    let set_atom_math = |table, name, f|
        set_atom_value(Some(table.unwrap_or(double_table)), name, value_util::snippet_closure(2, |x| match *x {
            [Value::Float(f1), Value::Float(f2)] => Value::Float(f(f1, f2)),
            [Value::Float(_), _] => ocaml::failwith("Don't know how to combine that with a number"),
            _ => unreachable!()
        }));

    let set_atom_test = |table, name, f|
        set_atom_value(Some(table.unwrap_or(double_table)), name, value_util::snippet_closure(2, |x| match *x {
            [Value::Float(f1), Value::Float(f2)] => value_util::bool_cast(f(f1, f2)),
            [Value::Float(_), _] => ocaml::failwith("Don't know how to compare that to a number"),
            _ => unreachable!()
        }));

    let set_atom_math_fn = |table, name, f|
        set_atom_fn(Some(table.unwrap_or(double_table)), name, |x| match x {
            Value::Float(f1) => Value::Float(f(f1)),
            _ => ocaml::failwith("Can only perform that function on a number")
        });

    set_atom_math(None, "add", |a, b| a + b);
    set_atom_math(None, "subtract", |a, b| a - b);
    set_atom_math(None, "multiply", |a, b| a * b);
    set_atom_math(None, "divide", |a, b| a / b);
    set_atom_math(None, "modulus", |a, b| (a % b + b) % b); /* Sign-of-divisor modulus */

    /* Do I really need all four comparators? */
    set_atom_test(None, "lessThan", |a, b| a < b);
    set_atom_test(None, "lessThanEqual", |a, b| a <= b);
    set_atom_test(None, "greaterThan", |a, b| a > b);
    set_atom_test(None, "greaterThanEqual", |a, b| a >= b);

    set_atom_math_fn(None, "floor", f64::floor);

    set_atom_fn(Some(double_table), "toString", |x| match x {
        Value::Float(f1) => Value::String(f1.to_string()),
        _ => ocaml::failwith("Can only perform that function on a number")
    });

    /* "Submodule" internal.string */
    let atom_table = insert_table(None, "atom");

    set_atom_fn(Some(atom_table), "toString", |x| match x {
        Value::Atom(ref s) => Value::String(s.into_owned()),
        _ => ocaml::failwith("Can only perform that function on an atom")
    });

    /* "Submodule" internal.string */
    let string_table = insert_table(None, "string");

    /* Note: Does NOT coerce into a type, f is of type f -> value */
    let set_atom_string_op = |table, name, f|
        set_atom_value(Some(table.unwrap_or(string_table)), name, value_util::snippet_closure(1, box |x| match *x {
            [Value::String(f1)] => f(f1),
            _ => ocaml::failwith("Can only perform that operation on a string")
        }));

    let uchar_to_codepoint = |u| Value::Float(u as f64);
    let uchar_to_string = |u| {
        let buffer = String::new();
        buffer.push(u as char);
        Value::String(buffer)
    };
    
    // takes an function "filter", and a char/string(?) "st"
    let iterator_value = |filter| box |st| {
        let loc = fake_register_location("internal.string.iterUtf8");
        
        Value::BuiltinHandoff(box |context, stack, (f, at)| match st.chars().nth(0) {
            Some(u) => {
                let result = filter(u);
                let mut v = stack.clone();
                v.push(fake_register_from(RegisterState::FirstValue(TRUEFN_VALUE, loc, loc.clone())));
                v.push(fake_register_from(RegisterState::PairValue(f, result, loc, loc.clone())));
                execute::execute_step(context, v)
            }
            _ => execute::return_to(context, stack, (Value::Null, loc))
        })
    };
    set_atom_string_op(None, "iterUtf8", iterator_value(uchar_to_string));
    set_atom_string_op(None, "iterUtf8Codepoint", iterator_value(uchar_to_codepoint));
    
    set_atom_value(Some(string_table), "codepointToString", value_util::snippet_closure(1, |x| match *x {
        [Value::Float(u)] => uchar_to_string(u as u32),
        _ => ocaml::failwith("Can only perform that operation on a number")
    }));

    set_atom_value(Some(string_table), "concat", value_util::snippet_closure(2, box |x| match *x {
        [Value::String(f1), Value::String(f2)] => Value::String(format!("{}{}", f1, f2)),
        [Value::String(_), _] => ocaml::failwith("Don't know how to combine that with a string"),
        _ => unreachable!()
    }));

    /* "Submodule" internal.type */
    let type_table = insert_table(None, "type");

    set_atom_fn(Some(type_table), "isAtom", |v| match v { Value::Atom(_) => Value::True, _ => Value::Null});
    set_atom_fn(Some(type_table), "isString", |v| match v { Value::String(_) => Value::True, _ => Value::Null});
    set_atom_fn(Some(type_table), "isNumber", |v| match v { Value::Float(_) => Value::True, _ => Value::Null});

    /* "Submodule" internal.type */
    /*
    if cfg!(BUILD_INCLUDE_C_FFI) {
        use ffi_support::*;
        let ffi_table = insert_table(None, "ffi");
        
        set_atom_fn(Some(ffi_table), "newForeign", |_| {
            let foreigner = ForeignWrap {
                name: None,
                args: vec![],
                returning: "void".to_owned()
            };
            let table = value_util::table_blank(TableBlankKind::NoSet);
            let set_ffi_param = |what, func| table.insert(Value::Atom(Cow::from(what)), Value::BuiltinFunction(box |a| match a {
                Value::String(s) => {
                    func(s.clone());
                    Value::Null
                }
                Value::Atom(ref c) => {
                    func(s.into_owned());
                    Value::Null
                }
                x => ocaml::failwith(&format!("Need key {} for ffi {}; expected string or atom", x, what)),
            }));
            set_ffi_param("name", |s| foreigner.name = Some(s));
            set_ffi_param("return", |s| foreigner.returning = s);
            set_ffi_param("args", |s| foreigner.args.push(s));
            
            table.insert(Value::Atom(Cow::from("make")), Value::BuiltinFunction(box |_|
                match foreigner.name {
                    None => ocaml::failwith("No name provided for FFI function"),
                    Some(name) =>
                        value_foreign(name, foreigner.args.iter().rev().cloned().collect(), foreigner.returning)
                }
            ));
            
            Value::Table(table)
        });
    }
    */
    /* Done */
});
}
