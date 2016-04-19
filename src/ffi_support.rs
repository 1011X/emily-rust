use value::Value;

pub struct ForeignWrap {
    name: Option<String>,
    args: Vec<String>,
    returning: String,
}

pub struct ValueToCFn<T>(T, Box<Fn(Value) -> T>);
pub struct CToValueFn<T>(T, Box<Fn(T) -> Value>);
/*
type valueToCFn = ValueToCFn -> valueToCFn
type cToValueFn = CToValueFn -> cToValueFn
*/
pub fn value_to_c_for<T>(s: &str) -> ValueToCFn<T> {
	match s {
		"void" => ValueToCFn((), box |_| ()),
		"int" => ValueToCFn(isize, box |x| match x {
	        Value::Float(f) => f as isize,
	        _ => ocaml::failwith("Expected number"),
	    }),
		"double" => ValueToCFn(f64, box |x| match x {
	        Value::Float(f) => f,
	        _ => ocaml::failwith("Expected number"),
	    }),
		"string" => ValueToCFn(*const u8, box |x| match x {
	        Value::String (s) => s,
	        _ => ocaml::failwith("Expected string"),
	    }),
		s => ocaml::failwith(&format!("Unsupported type name: {}", s)),
	}
}

pub fn c_to_value_for<T>(s: &str) -> CToValueFn<T> {
	match s {
		"void"   => CToValueFn((),     box |_| Value::Null),
		"int"    => CToValueFn(isize,  box |x| Value::Float (x as f64)),
		"double" => CToValueFn(f64,    box |x| Value::Float (x)),
		"string" => CToValueFn(String, box |x| Value::String (x)),
		s => ocaml::failwith(&format!("Unsupported type name for return: {}", s)),
	}
}

pub fn value_foreign_unary(name: String, arg_type_name: &str, ret_type, ret_convert) -> Value {
	let ValueToCFn(arg_type, arg_convert) = value_to_c_for(arg_type_name);
	//let ffi_binding = foreign name (arg_type @-> returning ret_type);
	
	Value::BuiltinFunction(box |arg| ret_convert(ffi_binding(arg_convert(arg))))
}

pub fn value_foreign(name: String, arg_type_names: Vec<String>, ret_type_name: String) -> Value {
	let CToValueFn(ret_type, ret_convert) = c_to_value_for(ret_type_name);
	
	match &*arg_type_names {
		[] =>  value_foreign_unary(name, "void", ret_type, ret_convert),
		[a] => value_foreign_unary(name, a,      ret_type, ret_convert),
		[a, b] => { /* "Arg type 0, arg convert 0..." */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ffi_binding = foreign name (at0 @-> at1 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
            	ret_convert (ffi_binding (ac0 a0) (ac1 a1))
        	) )
        }
    	[a, b, c] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
            	ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2))
        	) ) )
    	}
    	[a, b, c, d] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ValueToCFn (at3, ac3) = value_to_c_for(d);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> at3 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
		    Value::BuiltinFunction( fun a3 ->
            	ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2) (ac3 a3))
        	) ) ) )
    	}
		[a, b, c, d, e] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ValueToCFn (at3, ac3) = value_to_c_for(d);
		    let ValueToCFn (at4, ac4) = value_to_c_for(e);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> at3 @-> at4 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
		    Value::BuiltinFunction( fun a3 ->
		    Value::BuiltinFunction( fun a4 ->
		        ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2) (ac3 a3) (ac4 a4))
		    ) ) ) ) )
	    }
		[a, b, c, d, e, f] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ValueToCFn (at3, ac3) = value_to_c_for(d);
		    let ValueToCFn (at4, ac4) = value_to_c_for(e);
		    let ValueToCFn (at5, ac5) = value_to_c_for(f);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> at3 @-> at4 @-> at5 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
		    Value::BuiltinFunction( fun a3 ->
		    Value::BuiltinFunction( fun a4 ->
		    Value::BuiltinFunction( fun a5 ->
		        ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2) (ac3 a3) (ac4 a4) (ac5 a5))
		    ) ) ) ) ) )
	    }
		[a, b, c, d, e, f, g] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ValueToCFn (at3, ac3) = value_to_c_for(d);
		    let ValueToCFn (at4, ac4) = value_to_c_for(e);
		    let ValueToCFn (at5, ac5) = value_to_c_for(f);
		    let ValueToCFn (at6, ac6) = value_to_c_for(g);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> at3 @-> at4 @-> at5 @-> at6 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
		    Value::BuiltinFunction( fun a3 ->
		    Value::BuiltinFunction( fun a4 ->
		    Value::BuiltinFunction( fun a5 ->
		    Value::BuiltinFunction( fun a6 ->
		        ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2) (ac3 a3) (ac4 a4) (ac5 a5) (ac6 a6))
		    ) ) ) ) ) ) )
	    }
		[a, b, c, d, e, f, g, h] => { /* Each arity needs its own implementation currently */
		    let ValueToCFn (at0, ac0) = value_to_c_for(a);
		    let ValueToCFn (at1, ac1) = value_to_c_for(b);
		    let ValueToCFn (at2, ac2) = value_to_c_for(c);
		    let ValueToCFn (at3, ac3) = value_to_c_for(d);
		    let ValueToCFn (at4, ac4) = value_to_c_for(e);
		    let ValueToCFn (at5, ac5) = value_to_c_for(f);
		    let ValueToCFn (at6, ac6) = value_to_c_for(g);
		    let ValueToCFn (at7, ac7) = value_to_c_for(h);
		    let ffi_binding = foreign name (at0 @-> at1 @-> at2 @-> at3 @-> at4 @-> at5 @-> at6 @->at7 @-> returning ret_type);
		    Value::BuiltinFunction( fun a0 ->
		    Value::BuiltinFunction( fun a1 ->
		    Value::BuiltinFunction( fun a2 ->
		    Value::BuiltinFunction( fun a3 ->
		    Value::BuiltinFunction( fun a4 ->
		    Value::BuiltinFunction( fun a5 ->
		    Value::BuiltinFunction( fun a6 ->
		    Value::BuiltinFunction( fun a7 ->
		        ret_convert (ffi_binding (ac0 a0) (ac1 a1) (ac2 a2) (ac3 a3) (ac4 a4) (ac5 a5) (ac6 a6) (ac7 a7))
		    ) ) ) ) ) ) ) )
	    }
		_ => ocaml::failwith("Max ffi arguments currently 8")
	}
}
