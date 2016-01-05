/* Functions to execute a codeSequence */

/* The general approach to evaluate a group is: For each line:
    1. If the line is empty, skip it.
    2. Take the first token, remove it from the line, evaluate it, call that Value 1.
    3. If there are no more tokens in the line, the remaining Value 1 is the value of the line.
    4. Take the next remaining token, remove it from the line, evaluate it, call that Value 2.
    5. Apply Value 1 to Value 2, make the result the new Value 1. Goto 2.
    The value of the final nonempty line is the value returned from evaluating the group.
    Steps 2, 4 and 5 could potentially require code invocation, necessitating the stack.
 */

#![feature(slice_patterns, advanced_slice_patterns)]

use value;
use value_util;
use pretty;

use value::{
	ExecuteFrame,
	ExecuteStack,
	RegisterState,
	TableBlankKind,
	Value,
};

use value_util::BoxSpec;

use token::CodeSequence;

/* -- DEBUG / PRETTYPRINT HELPERS -- */

/* Pretty print for RegisterState. Can't go in pretty.rs because module recursion. */
pub fn dump_register_state(register_state: &RegisterState) -> String {
    match *register_state {
		RegisterState::LineStart (ref v, _) =>
			format!("LineStart:{}", v),
		RegisterState::FirstValue (ref v, ..) =>
			format!("FirstValue:{}", v),
		RegisterState::PairValue (ref v1, ref v2, ..) =>
			format!("PairValue:{},{}", v1, v2),
	}
}

/* FIXME: I wonder if there's a existing function for this in List or something. */
// lol
pub fn stack_depth(stack: ExecuteStack) -> usize {
	stack.len()
}

/* -- PRACTICAL HELPERS -- */

type AnchoredValue = (Value, CodePosition);

/* These three could technically move into value.rs but BuiltinObject depends on Value */
pub fn scope_inheriting(kind: TableBlankKind, scope_parent: Value) -> Value {
    Value::Table (value_util::table_inheriting(kind, scope_parent))
}

pub fn object_literal_scope(obj: BoxSpec, scope_parent: Value) -> Value {
    /* Should this be Value::Object or Value::Table, and *why*? */
    Value::Table (value_util::box_blank(obj, scope_parent))
}

/* Given a parent scope and a token creates an appropriate inner group scope */
pub fn group_scope(context: ExecuteContext, token_kind: TokenGroupKind, scope: Value, initializer_value: Option<Value>) -> Value {
    match token_kind {
        TokenGroupKind::Plain => initializer_value.unwrap_or(scope),
        TokenGroupKind::Scoped => scope_inheriting(
        	TableBlankKind::WithLet,
            initializer_value.unwrap_or(scope)
        ),
        TokenGroupKind::Box (kind) => object_literal_scope(
            value_util::PopulatingObject (initializer_value.unwrap_or(
            	value_util::object_blank(Some (context.object_proto))
            )),
            scope
        ),
    }
}

/* Combine a value with an existing register var to make a new register var. */
/* Flattens pairs, on the assumption if a pair is present we're returning their applied value, */
/* so only call if we know this is not a pair already (unless we *want* to flatten) */
pub fn new_state_for(register: RegisterState, av: AnchoredValue) -> RegisterState {
	match (register, av) {
    	/* Either throw out a stale LineStart / PairValue and simply take the new value, */
		(RegisterState::LineStart (_, rat), (v, at)) |
		(RegisterState::PairValue (_, _, rat, _), (v, at)) =>
			RegisterState::FirstValue (v, rat, at),
		/* Or combine with an existing value to make a pair. */
		(RegisterState::FirstValue (v, rat, _), (v2, at)) =>
			RegisterState::PairValue (v, v2, rat, at)
	}
}

/* Constructor for a new frame */
pub fn start_register(at: CodePosition) -> RegisterState {
	RegisterState::LineStart (Value::Null, at)
}

pub fn stack_frame(scope: Value, code: CodeSequence, at: CodePosition) -> ExecuteFrame {
	ExecuteFrame {
		register: start_register(at),
		code: code,
		scope: scope
	}
}

/* Only call if it really is impossible, since this gives no debug feedback */
/* Mostly I call this if a nested match has to implement a case already excluded */
// XXX Replaced by unreachable!()
/*
pub fn internal_fail() -> ! {
	panic!("Internal consistency error: Reached impossible place");
}
*/
pub fn fail_with_stack(stack: ExecuteStack, mesg: &str) -> Result <(), String> {
    Err (format!("{}\n{}", mesg, value_util::stack_string(stack)))
}

/* -- INTERPRETER MAIN LOOP -- */

/* ---- Structure notes:

The interpreter is a tree of mutually recursive functions:

executeStep: ("Proceed")
    | EXIT (Rare-- when entire program is empty)
    \ executeStepWithFrames: ("Evaluate first frame in stack")
        | apply (When register contains pair)
        \ evaluateToken: (When no pair, and we should check next token)
            | returnTo (When no token lines)
            \ evaluateTokenFromLines: ("Check first line of code after instruction pointer")
                | executeStep (When first line is empty)
                \ evaluateTokenFromTokens: ("Check first token in first line")
                    | apply (when evaluating word)
                    \ executeStep (when token evaluated and stack frame is adjusted with new register and/or new additional frame.)

returnTo: (A value has been calculated and a new stack top decided on; fit that value into the stack top's register.)
    | EXIT (when return from final frame)
    \ executeStep (to proceed with new register)

apply: (A pair of values has been identified; evaluate their application.)
    | returnTo (when application result can be calculated immediately)
    \ executeStep (when a closure or snippet requires a new frame)

*/

/* ---- Tail-call optimization notes:
    These are the recursion points for executeStep:
        evaluateTokenFromLines -> Line is present, but empty -> move to next
        returnTo -> top frame exists, a value was calculated by an outer frame, combine down onto it
        evaluateTokenFromTokens.simpleValue -> new value for this current frame known, just set it
        evaluateTokenFromTokens -> DESCENT: next token is a group; evaluate it.
        apply -> DESCENT: application is closure to value; make stack frame.
        apply -> DESCENT: application is hasValue or setValue; do in lower stack frame.
    In future, it might be useful to do the TCO stack rewriting at the descent points and
    blank-line removal at tokenize time, instead of wasting time on it each loop start.
*/

/* These first five functions are mostly routing: */
/* executeStep is the "start of the loop"-- the entry point we return to after each action.
   it currently just unpacks the stack, cleans it up, and passes components on to process. */
pub fn execute_step(context: ExecuteContext, stack: ExecuteStack) -> Value {
    match &*stack {
        /* Unusual edge case: Asked to execute an empty file -- just return */
        [] => Value::Null,

        /* Here some tail call optimization passes occur. We check for special stack patterns
           implying unnecessary space on the stack, then rewrite the stack to avoid them. */

        /* Case #1: Remove blank lines so they don't mess up other TCO checks later */
        [more_frames.., ExecuteFrame {register, scope, ref code, ..}]
        if code.len() >= 2 && code[1].is_empty() => execute_step(context, {
        	let mut v = more_frames.to_vec();
        	v.push(ExecuteFrame {
        		register: register,
        		scope: scope,
        		code: {
        			let mut c = code.clone();
        			c.remove(1);
        			c
        		},
        	});
        	v
        }),

        /* Case #2: A normal group descent, but into an unnecessary pair of parenthesis.
           IOW, the next-to-top frame does no work; its code only ever contained a group token. */
        [more_frames.., ExecuteFrame {
        	register: RegisterState::LineStart (..),
        	ref code, ..
        }, frame]
        if code.len() == 1 && code[0].is_empty() => execute_step(context, {
        	let mut v = more_frames.to_vec();
        	v.push(frame);
        	v
        }),

        /* Case #3: Canonical tail call: A function application, at the end of a group.
           We can thus excise the frame that's just waiting for the application to return. */
        [more_frames.., ExecuteFrame {
        	register: RegisterState::PairValue (..),
        	ref code, ..
        }, frame]
        if code.len() == 1 && code[0].is_empty() => execute_step(context, {
        	let mut v = more_frames.to_vec();
        	v.push(frame);
        	v
        }),

        /* Case #4: Applying a continuation: We can ditch all other context. */
        [more_frames.., frame @ ExecuteFrame {
	    	register: RegisterState::FirstValue (Value::Continuation (continue_stack, at), ..),
	        ref code, ..
		}]
		/* "Match a nonempty two-dimensional list" */
		if !code.is_empty() && !code[0].is_empty() => execute_step(context, {
        	let mut v = continue_stack.clone();
        	v.push(ExecuteFrame {
        		register: RegisterState::LineStart (Value::Null, at.clone()),
        		..frame
        	});
        	v
        }),

        /* Break stack frames into first and rest */
        [more_frames.., frame] =>
            execute_step_with_frames(context, stack, frame, more_frames.to_vec()),
    }
}

pub fn execute_step_with_frames(context: ExecuteContext, stack: ExecuteStack, frame: ExecuteFrame, more_frames: Vec<ExecuteFrame>) -> Value {
    /* Trace here ONLY if command line option requests it */
    if options::RUN.trace {
    	println!("    Step | Depth {}{} | State {} | Code {}",
			stack_depth(stack),
			if options::RUN.track_objects {
				format!(" | Scope {}", frame.scope)
			} else {
				"".to_string()
			},
			dump_register_state(frame.register),
			pretty::dump_code_tree_terse(
				token::make_group(
					Token {
						file_name: CodeSource::Unknown,
						line_number: 0,
						line_offset: 0
					},
					TokenClosureKind::NonClosure,
					TokenGroupKind::Plain,
					vec![],
					frame.code
				)
			)
		);
	}

    /* Check the state of the top frame */
    match frame.register {
        /* It has two values-- apply before we do anything else */
        RegisterState::PairValue (a, b, rat, bat) =>
            apply(context, stack, a, a, (b, bat)),

        /* Either no values or just one values, so let's look at the tokens */
        RegisterState::FirstValue (..) |
        RegisterState::LineStart (..) =>
            evaluate_token(context, stack, frame, more_frames)
            /* Pop current frame from the stack, integrate the result into the last frame and recurse (TODO) */
    }
}

pub fn evaluate_token(context: ExecuteContext, stack: ExecuteStack, frame: ExecuteFrame, more_frames: Vec<ExecuteFrame>) -> Value {
    /* Look at code sequence in frame */
    match &*frame.code {
        /* It's empty. We have reached the end of the group. */
        [] => {
        	let avalue = match frame.register { /* Unpack Value 1 from register */
                RegisterState::LineStart (v, rat) |
                RegisterState::FirstValue (v, rat, _) => (v, rat),
                _ => unreachable!(), /* If PairValue, should have branched off above */
            };
            /* "Return from frame" and recurse */
            return_to(context, more_frames, avalue)
        }

        /* Break lines in current frame's codeSequence into last and rest */
        [more_lines.., line] =>
            evaluate_token_from_lines(context, stack, frame, more_frames, line, more_lines),
    }
}

pub fn evaluate_token_from_lines(context: ExecuteContext, stack: ExecuteStack, frame: ExecuteFrame, more_frames: Vec<ExecuteFrame>, line: Vec<Token>, more_lines: Vec<Vec<Token>>) -> Value {
    /* Look at line in code sequence. */
    match &*line {
        /* It's empty. We have reached the end of the line. */
        [] => {
            /* Convert Value 1 to a LineStart value to persist to next line */
            let new_state = match frame.register {
                RegisterState::LineStart (v, rat) |
                RegisterState::FirstValue (v, rat, _) =>
                	RegisterState::LineStart (v, rat),
                _ => unreachable!(), /* Again: if PairValue, should have branched off above */
			};
            /* Replace current frame, new code sequence is rest-of-lines, and recurse */
            execute_step(context, {
            	let mut v = more_frames.clone();
            	v.push(ExecuteFrame {
		        	register: new_state,
		        	code: more_lines,
		        	scope: frame.scope
		        });
		        v
            })
		}
		
        /* Break tokens in current line into first and rest */
        [more_tokens.., token] =>
            evaluate_token_from_tokens(context, stack, frame, more_frames, line, more_lines, token, more_tokens)
    }
}

/* Enter a frame as if returning this value from a function. */
pub fn return_to(context: ExecuteContext, stack_top: ExecuteStack, av: AnchoredValue) -> Value {
    /* Trace here ONLY if command line option requests it */
    if options::RUN.trace {
    	println!("<-- {}", av.0);
    }

    /* Unpack the new stack. */
    match &*stack_top {
        /* It's empty. We're returning from the final frame and can just exit. */
        [] => av.0,

        /* Pull one frame off the stack so we can replace the register var and re-add it. */
        [past_return_frames.., ExecuteFrame {register: parent_register, code: parent_code, scope: parent_scope}] => {
            execute_step(context, {
            	let mut v = past_return_frames.to_vec();
            	v.push(ExecuteFrame {
		        	register: new_state_for(parent_register, av),
		        	code: parent_code,
		        	scope: parent_scope
		        });
		        v
            })
        }
    }
}

/* evaluateTokenFromTokens and apply are the functions that "do things"-- they
   define, ultimately, the meanings of the different kinds of tokens and values. */

pub fn evaluate_token_from_tokens(context: ExecuteContext, stack: ExecuteStack, frame: ExecuteFrame, more_frames: Vec<ExecuteFrame>, line: Vec<Token>, more_lines: Vec<Vec<Token>>, token: Token, more_tokens: Vec<Token>) -> Value {
    /* Helper: Given a value, and knowing register state, make a new register state and recurse */
    let stack_with_register = |register| {
    	let mut v = more_frames.clone();
    	v.push(ExecuteFrame {
			register: register,
			code: more_line.iter().cloned().chained(vec![more_tokens]).collect(),
			scope: frame.scope
		});
		v
	};

    let simple_value = |v| {
        /* ...new register state... */
        let new_state = new_state_for(frame.register, (v, token.at));
        /* Replace current line by replacing current frame, new line is rest-of-line, and recurse */
        execute_step(context, stack_with_register, new_state)
    };

    let closure_value = |v| {
        let (ret, key) = match v.closure {
        	TokenClosureKind::ClosureWithBinding (r, k) => (r, k),
        	_ => unreachable!(),
        };
        
        simple_value(Value::Closure (ClosureValue {
        	exec: ClosureExec::User {
        		body: v.items.clone(),
        		env_scope: frame.scope.clone(),
        		key: key,
        		scoped: v.kind == TokenGroupKind::Scoped,
        		has_return: ret
        	},
        	bound: vec![],
        	this: ClosureThis::Blank,
        	need_args: key.len()
        }))
    };

    /* Identify token */
    match token.contents {
        /* Straightforward values that can be evaluated in place: */
        /* A bare word should be looked up from the scope. */
        Token::Word (s) => apply(context, stack_with_register(frame.register), frame.scope, frame.scope, (Value::Atom (s), token.at)),
        /* A literal value should be simply converted from Token to Value. */
        Token::String (s) => simple_value(Value::String (s)),
        Token::Atom (s) => simple_value(Value::Atom (s)),
        Token::Number (f) => simple_value(Value::Float (f)),
        /* Symbols are not allowed at this point, they can only survive if a macro inserted one. */
        Token::Symbol (s) => token::fail_token(token, format!("Faulty macro: Symbol {} left unprocessed", s)),
        /* Not straightforward: This token is a parenthetical. */
        Token::Group (group) => match group.closure {
            /* Nonclosure groups are nontrivial to evaluate, and will require a new stack frame. */
            TokenClosureKind::NonClosure => {
                /* This creates the new frame with an enclosing scope designated. */
                let push_frame = |with_initializer_value| {
                    let new_scope = group_scope(context, group.kind, frame.scope, with_initializer_value);
                    let items = match group.kind {
                        TokenGroupKind::Box (_) => {
                            let wrapper_group = token::clone(token, TokenContents::Group (TokenGroup {
                            	kind: TokenGroupKind::Plain,
                            	closure: TokenGroupKind::NonClosure,
                            	group_initializer: vec![],
                            	items: group.items
                            }));
                            let word = clone(token, TokenContents::Word (value::CURRENT_KEY_STRING));
                            vec![vec![wrapper_group], vec![word]]
                        }
                        _ => group.items,
                    };
                   

                    /* Trace here ONLY if command line option requests it */
                    if options::RUN.trace {
                    	println!("Group --> {}", pretty::dump_value_new_table(new_scope));
                    }

                    /* New frame: Group descent */
                    execute_step(context, {
                    	let mut v = stack_with_register(frame.register).clone();
                    	v.push(stack_frame(new_scope, items, token.at));
                    	v
                    })
                };

                /* Now we need to pick that enclosing scope. */
                match &*group.group_initializer {
                    /* For ordinary groups, it is known: */
                    [] => push_frame(None),
                    /* But groups with an initializer, we must evaluate code to get the scope: */
                    group_initializer => {
                        let handoff = |f, _| push_frame(Some (f));
                        
                        execute_step(context, {
                        	let mut v = stack_with_register(RegisterState::FirstValue(Value::BuiltinHandoff (handoff), token.at, token.at));
                        	v.push(ExecuteFrame {
                            	register: start_register(token.at),
                                code: vec![group_initializer],
                                scope: frame.scope
                            });
                            v
                    	})
                	}
                }
            }
            /* Parenthetical is defining a new function. */
            _ => closure_value(group)
        },
    }
}

/* apply item a to item b and return it to the current frame */
pub fn apply(context: ExecuteContext, stack: ExecuteStack, this: Value, a: Value, b: AnchoredValue) -> Result<> {
    /* FIXME: Document what *exactly* is the definition of b/bat? */
    let (bv, bat) = b;
    let r = |v| return_to(context, stack, (v, bat));
    /* Pull something out of a table, possibly recursing */
    let read_table = |t| match (a, value::table_get(t, bv)) {
        (_, Some (Value::UserMethod (f))) =>
        	apply(context, stack, f, f, (this, bat)), /* FIXME: Comment this */
        (_, Some (Value::BuiltinMethod (f))) => r(Value::BuiltinFunction (f(this))),
        (_, Some (Value::BuiltinUnaryMethod (f))) => r(
            (try f(this) with
                Failure (e) => failWithStack stack @@ format!("Runtime error, applying {} to {}: {}", a, bv, e)
        )),
        (Value::Object (_), Some (c @ Value::Closure (_))) => r(value_util::raw_rethis_super_from(this, c)),
        (_, Some (v)) => r(v),
        (_, None) => match (a, value::table_get(t, value::PARENT_KEY)) {
            (Value::Object (_), Some (parent @ Value::Closure (_))) =>
                apply(context, stack, this, value_util::raw_rethis_super_from(this, parent), b),
            (_, Some (parent)) => apply(context, stack, this, parent, b),
            (_, None) => value_util::raw_misapply_stack(stack, this, bv),
        },
    };
    /* Unpack prototypes from context */
    let ExecuteContext {
    	null_proto,
    	true_proto,
    	float_proto,
    	string_proto,
    	atom_proto,
    	object_proto
    } = context;
    /* Perform the application */
    match a {
        /* If applying a closure. */
        Value::Closure (c) => {
            let descend = |c| {
                let b = c.bound.iter().cloned().rev().collect::<Vec<_>>();
                
                match c.exec {
                    ClosureExec::User (exec) => {
                        /* FIXME: should be a noscope operation for bound=[], this=None */
                        let scope_kind = if exec.scoped {
                        	TableBlankKind::WithLet
                        } else {
                        	TableBlankKind::NoLet
                        };
                        let scope = scope_inheriting(scope_kind, exec.env_scope);
                        let key = exec.key;
                        
                        /* Trace here ONLY if command line option requests it */
                        if options::RUN.trace {
                        	println!("Closure --> {}", pretty::dump_value_new_table(scope));
                        }

                        if let Value::Table(mut ref t) = scope {
                            fn add_bound(keys: Vec<String>, values: Vec<Value>) {
                                match (&*keys, &*values) {
                                    ([], []) => {}
                                    ([rest_key.., key], [rest_value.., value]) => {
                                        t.insert(Value::Atom (key), value);
                                        add_bound(rest_key, rest_value);
                                    }
                                    _ => unreachable!(),
                            	}
                            }
                            
                            let set_this = |current, this| {
                                t.insert(value::CURRENT_KEY.clone(), current);
                                t.insert(value::THIS_KEY.clone(), this);
                                t.insert(value::SUPER_KEY.clone(), value_util::make_super(current, this));
                            };
                            
                            match c.this {
                                ClosureThis::Current(c, t) |
                                ClosureThis::Frozen(c, t) => set_this(c, t),
                                _ => {}
                            }
                            
                            if let ClosureExec::User {has_return: true, ..} = c.exec {
                            	t.insert(value::RETURN_KEY.clone(), Value::Continuation (stack, bat));
                            }
                            
                            add_bound(key, bound);
                        } else {
                        	unreachable!();
                        }
                        
                        execute_step(context, stack.iter().cloned().chained(vec![stack_frame(scope, exec.body, bat)]).collect())
                    }
		            ClosureExec::Builtin (f) =>
		                r(try (f bound) with
		                    Failure e => failWithStack stack @@ "Runtime error, applying builtin closure to arguments ["^ (String.concat ", " @@ List.map Pretty.dumpValue bound) ^"]: " ^ e)
                }
            };
            
            match c.need_args {
                0 => descend(c), /* Apply discarding argument */
                count => {
                    let amended_closure = ClosureValue {
                    	need_args: count - 1,
                        bound: c.bound.iter().cloned().chained(vec![bv]).collect(),
                        ..c
                    };
                    match count {
                        1 => descend(amended_closure), /* Apply, using argument */
                        _ => r(Value::Closure (amended_closure)), /* Simply curry and return. Don't descend stack. */
                    }
                }
            }
        }

        Value::Continuation (stack, _) => /* FIXME: Won't this be optimized out? */
            return_to(context, stack, b),

        /* If applying a table or table op. */
        Value::Object (t) | Value::Table (t) => read_table(t),
        
        /* If applying a primitive value. */
        Value::Null =>       apply(context, stack, a, null_proto, b),
        Value::True =>       apply(context, stack, a, true_proto, b),
        Value::Float (v) =>  apply(context, stack, a, float_proto, b),
        Value::String (v) => apply(context, stack, a, string_proto, b),
        Value::Atom (v) =>   apply(context, stack, a, atom_proto, b),
        
        /* If applying a builtin special. */
        Value::BuiltinFunction (f) => r(match f(bv) {
            Err(ocaml::Failure (e)) =>
            	fail_with_stack(stack, &format!("Runtime error, applying builtin function to {}: {}", bv, e)),
            Err(e) => return Err(e),
            Ok(v) => v,
        }),
        Value::BuiltinHandoff (f) => /* Note: No this included, so not for methods */
            f(context, stack, b),
        /* Unworkable -- all builtin method values should be erased by readTable */
        Value::BuiltinMethod (_) | Value::BuiltinUnaryMethod (_) | Value::UserMethod (_) =>
        	unreachable!(),
    }
}

/* --- MAIN LOOP ENTRY POINT --- */

pub fn execute(starting_point: ExecuteStarter, code: Token) -> Value {
    let ExecuteStarter {context, root_scope} = starting_point;
    let initial_frame = stack_frame(root_scope, vec![vec![code]], code.at);
    execute_step(context, vec![initial_frame]) /* then place it as the start of the stack. */
}
