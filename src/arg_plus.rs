/* This is a reimplementation of Arg, with the following differences:

	- Arguments may be specified with an =, like, --key=value
	- Rule functions may throw a Complete to succeed while terminating further parsing
	- `Help of string` complemented by `Help of int` (exit code)
	- Provisions for environment variables

Not all features of Arg are implemented, notably most spec types are not supported. */

use std::env;
use std::collections::HashMap;

/* I use this to turn a single set of rules into simultaneously environment and argument parse rules. See options.ml */
pub fn key_mutate<F, V>(f: F, vec: V) -> V where
F : FnOnce(Vec<Arg::key>) -> Vec<Arg::key>,
V : Vec<(Vec<Arg::key>, Arg::spec, Arg::doc)> {
	vec.iter().cloned().map(|(a, b, c)| (f(a), b, c)).collect()
}

fn argPlusLimitations(who: &'static str) -> Result<(), ocaml::Failure> {
	Err (ocaml::Failure (format!("Internal error: Called {} with an arg spec it was not designed to handle.", who)))
}

/* Rule methods can raise Arg.Bad, Arg.Help, ArgPlus.Help or ArgPlus.Complete */

// exceptions
struct Complete;   /* Success, stop processing arguments */
struct Help (i32); /* Argument is exit code */

/* Takes the rule list normally given as first argument to Arg.parse and parses env vars against it. */
fn envParse(arg: Vec<(Arg.key, Arg.spec, Arg.doc)>) -> {
	for (key, spec, _) in arg {
		/* Rather than iterating env, iterate the rule list and check for each env we recognize */
		let value = match env::var(key) { /* May fail */
			Ok (v) => v,
			Err (_) => continue /* env::var failed, which means the env var wasn't present. Move on */
		}
		
		match spec {
			/* Discard argument-- does this ever even make sense? */
			Arg.Unit (f) => f(),

			/* String argument */
			Arg.String (f) => f(value),

			/* Incorrect use of ArgParse */
			_ => argPlusLimitations("envParse")
		}
	}
}

/* Notice one additional argument vs Arg.parse, called after successful completion with unprocessed part of arg list (possibly empty) */
fn argParse(rules, fallback, usage, onComplete) {
	/* Store all rules keyed on parameter */
	let lookup : HashMap<Arg::Key, Arg::Spec> = HashMap::with_capacity(1);
	for (key, spec, _) in rules {
		lookup.insert(key, spec);
	}

	/* Function to imperatively stream in each argument, one at a time */
	let mut rest : Vec<String> = env::args().collect();
	let consume = || match &*rest {
		[] => None,
		[next, more..] => {
			rest = more.to_vec();
			Some(next)
		}
	};

	/* Inner loop */
	fn proceed() {
		if let Some (key) = consume() { /* Argument found */
			match lookup.get(key) {
				/* This is a known argument and it has no arguments */
				Some (Arg.Unit (f)) => Ok(f()),

				/* This is a known argument and it has one argument, a string */
				Some (Arg.String (f)) => match consume() {
					None => Err (Arg.Bad (format!("option '{}' needs an argument.", key))),
					Some (arg) => Ok (f(arg))
				},

				/* Incorrect use of ArgPlus */
				Some (_) => argPlusLimitations("argParse"),

				/* Not a known argument key */
				None => {
					/* Interpret key string to see just what this is */
					let keyLen = key.len();

					/* It starts with a - */
					if keyLen > 0 && key.chars().nth(0) == '-' {
						match key.find("=") {
							/* It's a --a=b, which is why we didn't find the key in the lookup table... */
							Some(splitAt) => {
								/* Split out the key and value implied by the = and take a pass at lookup */
								let subKey = key[0..splitAt].to_string();
								let subValue = key[splitAt + 1 .. keyLen - splitAt - 1].to_string();
								match lookup.get(subKey) {
									/* The argument is recognized, but can't be used with = */
									Some (Arg.Unit (_)) => Err (Arg.Bad (format!("option '{}' does not take an argument.", subKey)))

									/* The argument is recognized and we can work with it */
									Some (Arg.String (f)) => Ok(f(subValue)),

									/* Incorrect use of ArgPlus */
									Some (_) => argPlusLimitations("argParse"),

									/* Despite pulling out the =, it's still unrecognized */
									None => Err (Arg.Bad (format!("unknown option '{}'", subKey)))
								}
							},
							/* No gimmicks, it's just plain not recognized */
							None => Err (Arg.Bad (format!("unknown option '{}'", key)))
						}
					}
					/* This doesn't start with a -, so it's an anonymous argument. Let the user handle it */
					else {
						fallback(key);
					}
					proceed();
				}
			}
		}
	};

	/* Error/exceptional situation handling */
	let name = match consume() {
		Some (s) => s, /* argv[0] is executable name */
		None => "(INTERNAL ERROR)".to_string() /* None implies argc==0, so we won't ever be displaying this anyway? */
	};
	match {
		match proceed() {
			/* An arg rule requested the help be shown using the Arg interface. */
			Err (Arg.Help (_)) => Err (Help 0), /* FIXME: What is the argument to Arg.Help for? It isn't documented. */

			/* An arg rule requested a premature halt to processing. */
			Err (Complete) => Ok(()),
			
			/* We ended without failing, so call the complete handler */
			_ => onComplete(!rest)
		}
	} {
		/* Something requested the help be shown */
		| Help i -> Arg.usage rules usage; exit i

		/* Something requested we flag failure to the user */
		| Arg.Bad s -> prerr_endline @@ name^": "^s; Arg.usage rules usage; exit 1
	}
}
