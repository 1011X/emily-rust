/* Set up environment and invoke Execute */

use value::{
	Value,
	ExecuteStarter,
	TableType,
	TableBlankKind,
};

/* File handling utilities */

/* Convert a filename to an atom key for a loader */
/* FIXME: Refuse to process "unspeakable" atoms, like "file*name"? */
pub fn nameAtom(filename) -> Value {
	Value::Atom (try
        Filename.chop_extension filename      /* If there is an extension remove it */
    with
        Invalid_argument _ -> filename)       /* If there is no extension do nothing */
}

/* See also path.ml */
let default_package_path = {
    let env_path = env!("BUILD_PACKAGE_DIR");
    path::executable_relative_path(env_path)
};

pub fn package_root_path() {
    match options::RUN.package_path {
    	Some(s) => s,
    	_ => default_package_path,
	}
}

/* What should the target of this particular loader be? */
pub enum LoaderSource {
    NoSource,                  /* I don't want the loader */
    SelfSource,                /* I want the loader inferred from context */
    Source(Value),             /* I want it to load from a specific path */
}

/* From what source should the project/directory loaders for this execution come? */
pub enum LoadLocation {
    Cwd,           /* From the current working directory */
    Path(String),  /* From a known location */
}

/* Given a loaderSource and a known context path, eliminate the SelfSource case */
pub fn self_filter(self: Value, source: LoaderSource) -> {
	match source {
		LoaderSource::SelfSource => LoaderSource::Source(self),
		_ => source,
	}
}

/* Given a pre-selfFiltered loaderSource, convert to an option. */
/* FIXME: Should the error state (SelfSource) instead be an allowed case? */
pub fn known_filter(source: LoaderSource) -> Option<Value> {
	match source {
		LoaderSource::NoSource => None,
		LoaderSource::Source(x) => Some(x),
		_ => ocaml::failwith("Internal error: Package loader attempted to load a file as if it were a directory".to_string()),
	}
}
/* There is one "base" starter plus one substarter for each file executed.
   The base starter lacks a project/directory, the others have it. */

/* Given a starter, make a new starter with a unique scope that is the child of the old starter's scope.
   Return both the starter and the new scope. */
/* THIS COMMENT IS WRONG, FIX IT */
pub fn sub_starter_with(starter: ExecuteStarter, table: TableType) -> ExecuteStarter {
    ExecuteStarter {
    	root_scope: Value::Table(table),
    	..starter
	}
}

pub fn sub_starter_pair(kind: Option<TableBlankKind>, starter: ExecuteStarter) -> (TableType, ExecuteStarter) {
    let table = value_util::table_inheriting(kind.unwrap_or(TableBlankKind::NoLet), starter.root_scope);
    (table, sub_starter_with(starter, table))
}

pub fn box_sub_starter(starter: ExecuteStarter, kind: BoxSpec) -> ExecuteStarter {
    sub_starter_with(starter, value_util::box_blank(kind, starter.root_scope))
}

/* Given a starter, make a new starter with a subscope and the requested project/directory. */
pub fn starter_for_execute(starter: ExecuteStarter, project: Option<Value>, directory: Option<Value>) -> ExecuteStarter {
    let (table, sub_starter) = sub_starter_pair(None, starter);
    value::table_set_option(table, value::PROJECT_KEY, project);
    table.insert(value::DIRECTORY_KEY, match directory {
		Some(d) => d,
		None => Value::Table(value_util::table_blank(TableBlankKind::NoSet)),
	});
    sub_starter
}

/* Loader is invoking execute internally to load a package from a file. */
pub fn execute_package(starter: ExecuteStarter, project: Option<Value>, directory: Option<Value>, buf) -> {
    execute::execute(starter_for_execute, starter, project, directory, buf)
}

lazy_static! {
	pub static mut ref PACKAGES_LOADED: HashMap<> = HashMap::new();
}

/* Create a package loader object. Will recursively call itself in a lazy way on field access.
   Directory and projectSource will be replaced as needed on field access, will not. */
/* TODO: Consider a guard on multi-loading? */
/* TODO: Here "lazy" means if you access a field corresponding to a file, the file is loaded.
         Maybe loading should be even lazier, such that load when a field is loaded *from* a file, load occurs?
         This would make prototype loading way easier. */
/* FIXME: Couldn't kind be NewScope and the starter impose the box? */
pub fn load_file(starter: ExecuteStarter, project_source: LoaderSource, directory: LoaderSource, path) -> {
    /* FIXME: What if known_filter is NoSource here? This is the "file where expected a directory" case. */
    let buf = tokenize::tokenize_channel(CodeSource::File(path), open_in(path));
    execute_package(starter, known_filter(project_source), known_filter(directory), buf)
}

pub fn load_package_dir(starter: ExecuteStarter, project_source: LoaderSource, path) -> Value {
    let directory_table = value_util::table_blank(TableBlankKind::NoSet);
    let directory_object = Value::Object(directory_table);
    let directory_filter = self_filter(directory_object);
    let proceed = load_package(starter, directory_filter(project_source), LoaderSource::Source(directory_object));
    for name in Sys.readdir(path) {
        value_util::table_set_lazy(directory_table, name_atom(name), |_| proceed(Filename.concat path name));
	}
    directory_object
}

pub fn load_package(starter: ExecuteStarter, project_source: LoaderSource, directory: LoaderSource, path) -> {
    /* This is gonna do bad things on case-insensitive filesystems */
    match packages_loaded.get(path) {
        Some(v) => v,
        None => {
            let v =
                try
                    /* COMMENT ME!!! This is not good enough. */
                    if path.len() == 0 {
                        raise @@ Sys_error "Empty path"
                    }
                    else if Sys.is_directory path {
                        load_package_dir(starter, project_source, path)
                    }
                    else {
                        let package_scope = Value::Table(value_util::table_blank(TableBlankKind::NoSet));
                        ignore @@ load_file (box_sub_starter starter @@ Value_util.(Populating(Package,package_scope)))
                            project_source directory path;
                        package_scope
                    }
                with Sys_error s ->
                    Value::Table(value_util::table_blank(TableBlankKind::NoSet))
            packages_loaded.insert(path, v);
            v
        }
    }
}

/* Return the value for the project loader. Needs to know "where" the project is. */
pub fn project_path_for_location(location: LoadLocation) -> {
    match options::RUN.project_path {
        Some(s) => s,
        None => match location {
        	Cwd => path::BOOT_PATH,
        	Path(st) -> st,
    	},
    }
}

pub fn project_for_location(starter: ExecuteStarter, default_location: LoadLocation) -> {
    load_package(starter, LoaderSource::SelfSource, LoaderSource::NoSource, project_path_for_location(default_location))
}

/* For external use: Given a file, get the load_location it would be executed within. */
pub fn location_around(path) -> {
    Path (Filename.dirname path)
}

/* External entry point: Build a starter */
pub fn complete_starter(with_project_location: LoadLocation) -> ExecuteStarter {
    let root_scope = value_util::table_blank(TableBlankKind::NoSet);
    let nv = || Value::Table(value_util::table_blank(TableBlankKind::NoSet)); /* "New value" */
    let package_starter = ExecuteStarter {
    	root_scope: Value::Table(root_scope),
    	context: ExecuteContext {
		    null_proto: nv(),
		    true_proto: nv(),
		    float_proto: nv(),
		    string_proto: nv(),
		    atom_proto: nv(),
		    object_proto: nv(),
	    },
    };
    let package_path = package_root_path();
    let package = load_package(package_starter, LoaderSource::NoSource, LoaderSource::NoSource, package_path);
    let populate_proto = |proto, path_key| {
        /* TODO convert path to either path or value to load from  */
        /* TODO find some way to make this not assume path loaded from disk */
        let path = vec!["emily", "core", "prototype", PATH_KEY + ".em"].into_iter().fold(package_path, FilePath.concat);
        let enclosing = load_package_dir(package_starter, LoaderSource::NoSource, Filename.dirname path);
        load_file(
            box_sub_starter(package_starter, BoxSpec::Populating(BoxTarget::Package, proto)),
            LoaderSource::NoSource,
            LoaderSource::Source(enclosing),
            path,
        );
    };
    
    root_scope.insert(value::INTERNAL_KEY, internal_package::INTERNAL_VALUE);
    root_scope.insert(value::PACKAGE_KEY, package);
    populate_proto(package_starter.root_scope,           "scope");
    populate_proto(package_starter.context.null_proto,   "null");
    populate_proto(package_starter.context.true_proto,   "true");
    populate_proto(package_starter.context.float_proto,  "number");
    populate_proto(package_starter.context.string_proto, "string");
    populate_proto(package_starter.context.atom_proto,   "atom");
    populate_proto(package_starter.context.object_proto, "object");
    let project = project_for_location(package_starter, with_project_location);
    let (scope, starter) = sub_starter_pair(Some(TableBlankKind::WithLet), package_starter);
    scope.insert(value::PROJECT_KEY,   project);
    scope.insert(value::DIRECTORY_KEY, project);
    starter
}

/* External entry point: Given a starter and a buffer, execute it */
pub fn execute_program_from(location: LoadLocation, buf) -> {
    execute::execute(complete_starter(location), buf)
}
