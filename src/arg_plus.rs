/* This is a reimplementation of Arg, with the following differences:

	- Arguments may be specified with an =, like, --key=value
	- Rule functions may throw a Complete to succeed while terminating further parsing
	- `Help of string` complemented by `Help of int` (exit code)
	- Provisions for environment variables

Not all features of Arg are implemented, notably most spec types are not supported. */

use std::env;
use std::collections::HashMap;

use ocaml::arg;
use ocaml::arg::Spec;

/* I use this to turn a single set of rules into simultaneously environment and argument parse rules. See options.ml */
pub fn key_mutate<F>(f: F, vec: Vec<(Vec<arg::Key>, arg::Spec, arg::Doc)>) -> Vec<(Vec<arg::Key>, arg::Spec, arg::Doc)> where
F: FnOnce(Vec<arg::Key>) -> Vec<arg::Key> {
	vec.iter().cloned().map(|(a, b, c)| (f(a), b, c)).collect()
}

fn arg_plus_limitations(who: &'static str) -> Result<(), ocaml::Failure> {
	ocaml::failwith(format!("Internal error: Called {} with an arg spec it was not designed to handle.", who))
}

/* Rule methods can raise Arg.Bad, Arg.Help, ArgPlus.Help or ArgPlus.Complete */
pub enum Error {
	Bad (String),
	Help (String),
	HelpExit (i32), /* Argument is exit code */
	Complete,       /* Success, stop processing arguments */
}

/* Takes the rule list normally given as first argument to Arg.parse and parses env vars against it. */
pub fn env_parse(arg: Vec<(arg::Key, arg::Spec, arg::Doc)>) -> Result<(), ocaml::Failure> {
	for (key, spec, _) in arg {
		/* Rather than iterating env, iterate the rule list and check for each env we recognize */
		let value = match env::var(key) { /* May fail */
			Ok (v) => v,
			Err (_) => continue, /* env::var failed, which means the env var wasn't present. Move on */
		};
		
		match spec {
			/* Discard argument-- does this ever even make sense? */
			Spec::Unit (f) => f(),

			/* String argument */
			Spec::String (f) => f(value),

			/* Incorrect use of ArgParse */
			_ => return arg_plus_limitations("env_parse"),
		}
	}
	Ok(())
}

/* Notice one additional argument vs Arg.parse, called after successful completion with unprocessed part of arg list (possibly empty) */
pub fn arg_parse<F, G, V>(rules: V, fallback: F, usage: String, on_complete: G) where
V: Vec<(arg::Key, arg::Spec, arg::Doc)>,
F: Fn(String) -> Result<(), Error>,
G: Fn(Vec<String>) {
	/* Store all rules keyed on parameter */
	let lookup: HashMap<Arg::Key, Arg::Spec> = HashMap::with_capacity(rules.len());
	
	for (key, spec, _) in rules {
		lookup.insert(key, spec);
	}

	/* Function to imperatively stream in each argument, one at a time */
	let mut rest = env::args();

	/* Inner loop */
	// () -> Result<(), >
	let proceed = || while let Some (key) = rest.next() { /* Argument found */
		match lookup.get(&key) {
			/* This is a known argument and it has no arguments */
			Some (Spec::Unit (f)) => return Ok (f()),

			/* This is a known argument and it has one argument, a string */
			Some (Spec::String (f)) => return match rest.next() {
				None => Err (Error::Bad (format!("option '{}' needs an argument.", key))),
				Some (arg) => Ok (f(arg)),
			},

			/* Incorrect use of ArgPlus */
			// FIXME: Handle Result value here
			Some (_) => return arg_plus_limitations("arg_parse"),

			/* Not a known argument key */
			None => {
				/* Interpret key string to see just what this is */

				/* It starts with a - */
				if key.len() > 0 && key.starts_with('-') {
					match key.find('=') {
						/* It's a --a=b, which is why we didn't find the key in the lookup table... */
						Some (split_at) => {
							/* Split out the key and value implied by the = and take a pass at lookup */
							let sub_key = &key[..split_at];
							let sub_value = key[split_at + 1..].to_string();
							
							match lookup.get(sub_key) {
								/* The argument is recognized, but can't be used with = */
								Some (Spec::Unit (_)) => Err (Spec::Bad (format!("option '{}' does not take an argument.", sub_key))),

								/* The argument is recognized and we can work with it */
								Some (Spec::String (f)) => Ok (f(sub_value)),

								/* Incorrect use of ArgPlus */
								Some (_) => arg_plus_limitations("arg_parse"),

								/* Despite pulling out the =, it's still unrecognized */
								None => Err (Spec::Bad (format!("unknown option '{}'", sub_key)))
							}
						}
						/* No gimmicks, it's just plain not recognized */
						None => Err (Spec::Bad (format!("unknown option '{}'", key)))
					}
				}
				/* This doesn't start with a -, so it's an anonymous argument. Let the user handle it */
				else {
					fallback(key);
				}
			}
		}
	};

	/* Error/exceptional situation handling */
	/* argv[0] is executable name */
	/* None implies argc==0, so we won't ever be displaying this anyway? */
	let name = rest.next().unwrap_or("(INTERNAL ERROR)".to_string());
	
	match {
		match proceed() {
			/* An arg rule requested the help be shown using the Arg interface. */
			Err (ocaml::Error::Help (_)) => Err (Error::HelpExit (0)), /* FIXME: What is the argument to Arg.Help for? It isn't documented. */

			/* An arg rule requested a premature halt to processing. */
			Err (Complete) => Ok (()),
			
			/* We ended without failing, so call the complete handler */
			_ => on_complete(!rest)
		}
	} {
		/* Something requested the help be shown */
		Error::Help(i) => {
			Arg::usage(rules, usage);
			exit(i);
		}

		/* Something requested we flag failure to the user */
		Error::Bad(s) => {
			writeln!(std::io::stderr(), "{}: {}", name, s);
			Arg::usage(rules, usage);
			exit(1);
		}
	}
}
