use crate::common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 ~> __integer_abs__").expect("5");
}

// === Type-consuming builtins (`__data_decode__<'t>`) ====================================
// A type-consuming builtin's behavior depends on its explicit type argument: the compiler
// resolves it to a concrete type id, embeds it in the emitted instruction, and the value
// carries it to the eventual call — through bindings, generic boundaries, captures, and
// process spawns — where the implementation reads it from the context.

#[test]
fn test_type_argument_arity_is_checked() {
    // A direct *application* of the bare builtin without the type argument is a compile
    // error…
    quiver()
        .evaluate(r#"__data_decode__ "5""#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsRequired {
            builtin: "data_decode".to_string(),
            parameter: "t".to_string(),
        });
    quiver()
        .evaluate(r#"__data_decode__<'int, 'bin>"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsTooMany {
            declared: 1,
            given: 2,
        });
}

#[test]
fn test_type_argument_rides_the_value() {
    // A reference captures the instantiation; the later call reads it.
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; f "5""#)
        .expect("5");
    // Through a *generic* declared boundary: the static type sheds the instantiation,
    // the value keeps it — the transport the wire-decode design relies on.
    quiver()
        .evaluate(
            r#"call = #<'u>[#'%str -> 'u] { $.0 "42" }
               [__data_decode__<'int>] ~> call"#,
        )
        .expect("42");
    // Into a spawned process, by capture and by init argument.
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; p = @#[] { f "42" } []; !p"#)
        .expect("42");
    quiver()
        .evaluate(
            r#"'decoder = #'%str -> ('int | [])
               w = #'decoder { $ "7" }
               p = __data_decode__<'int> ~> @w; !p"#,
        )
        .expect("7");
}

#[test]
fn test_type_argument_participates_in_equality() {
    // The type argument is operational (unlike annotations), so differently-instantiated
    // builtins are unequal and same-instantiated ones equal. (Referenced inside tuples —
    // a bare callable in a value position would be *called*.)
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; g = __data_decode__<'bin>; [f] ~> { =[^g] => Equal | Different }"#)
        .expect("Different");
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; g = __data_decode__<'int>; [f] ~> { =[^g] => Equal | Different }"#)
        .expect("Equal");
}

// === Several type parameters, the trailing ones inferred ================================
// A type-consuming builtin may declare several type parameters. An application writes a
// prefix of them and the rest are inferred from its argument, as an ordinary generic's are;
// each must come out concrete. No shipped builtin has two yet, so these compile against a
// registry carrying a probe: `__probe__<'a, 'b>` takes a `'b` and answers both by name.

mod probe {
    use quiver_compiler::compiler::{Bindings, CompileOptions, ModuleCache, SessionTables};
    use quiver_core::builtins::{BuiltinContext, BuiltinRegistry, Completion, Purity, TypeSpec};
    use quiver_core::bytecode::Function;
    use quiver_core::error::Error;
    use quiver_core::program::Program;
    use quiver_core::types::{Type, TypeLookup};
    use quiver_core::value::Value;
    use quiver_io::NativeEffect;
    use std::collections::HashMap;

    fn probe(
        _argument: &Value,
        ctx: &mut BuiltinContext<NativeEffect>,
    ) -> Result<Completion<NativeEffect>, Error> {
        let names: Vec<String> = ctx
            .type_arguments()
            .iter()
            .map(|&id| quiver_core::format::format_type_by_id(&*ctx.executor, id))
            .collect();
        let Some(Type::Tuple(pair)) = ctx
            .result_type()
            .and_then(|result| ctx.executor.lookup_type(result))
            .cloned()
        else {
            panic!("the probe answers a pair");
        };
        let mut fields = Vec::new();
        for name in names {
            fields.push(Value::Binary(
                ctx.executor.allocate_binary(name.into_bytes())?,
            ));
        }
        Ok(Completion::Value(Value::tuple(pair, fields)))
    }

    fn builtins() -> BuiltinRegistry<NativeEffect> {
        let mut builtins = BuiltinRegistry::<NativeEffect>::with_modules(
            &quiver_core::builtins::universal_modules(),
        );
        builtins.register_generic(
            "probe".to_string(),
            probe,
            Purity::Pure,
            TypeSpec::Var("b"),
            TypeSpec::Tuple(
                None,
                vec![(None, TypeSpec::Binary), (None, TypeSpec::Binary)],
            ),
            vec!["a".to_string(), "b".to_string()],
        );
        builtins
    }

    /// Compile and run `source` against the probe's registry, answering the result (as
    /// binaries, read as text) or the compile error.
    pub fn run(source: &str) -> Result<Vec<String>, quiver_compiler::compiler::Error> {
        let builtins = builtins();
        let parsed = quiver_compiler::parse(source).expect("parse");
        let resolver = quiver_compiler::PackageResolver::memory(HashMap::new());
        let mut program = Program::new();
        let mut module_cache = ModuleCache::new();
        let nil = program.register_type(Type::nil());
        let compiled = quiver_compiler::Compiler::compile(
            parsed,
            &Bindings::default(),
            SessionTables::default(),
            &mut module_cache,
            &resolver,
            &mut program,
            nil,
            &HashMap::new(),
            &builtins,
            None,
            CompileOptions::default(),
        )
        .map_err(|located| located.error)?;
        let never = program.never();
        let callable = program.register_type(Type::Callable {
            parameter: nil,
            result: compiled.result_type,
            receive: never,
            states: None,
            omittable: Vec::new(),
        });
        let entry = program.register_function(Function {
            instructions: compiled.instructions,
            captures: 0,
            type_id: callable,
        });
        let (value, mut executor) = quiver_core::execute_bytecode_sync_with(
            program.to_bytecode(Some(entry)),
            &builtins,
            1_000_000,
            None,
        )
        .expect("execution");
        let Value::Tuple(_, fields) = value else {
            panic!("expected the probe's pair, found {value:?}");
        };
        Ok(fields
            .iter()
            .map(|field| {
                let Value::Binary(binary) = field else {
                    panic!("expected a binary");
                };
                String::from_utf8(executor.get_binary_data(binary).unwrap().to_vec()).unwrap()
            })
            .collect())
    }
}

#[test]
fn test_trailing_type_arguments_are_inferred_from_the_argument() {
    assert_eq!(
        probe::run("__probe__<'int> <0a>").unwrap(),
        ["'int", "'bin"]
    );
}

#[test]
fn test_every_type_argument_may_be_written() {
    // Written, `'b` is checked against the argument rather than inferred from it.
    assert_eq!(
        probe::run("__probe__<'int, 'bin> <0a>").unwrap(),
        ["'int", "'bin"]
    );
    assert!(probe::run("__probe__<'int, 'int> <0a>").is_err());
}

#[test]
fn test_a_parameter_the_argument_does_not_determine_must_be_written() {
    assert_eq!(
        probe::run("__probe__ <0a>").unwrap_err(),
        quiver_compiler::compiler::Error::TypeArgumentsRequired {
            builtin: "probe".to_string(),
            parameter: "a".to_string(),
        }
    );
}

#[test]
fn test_an_inferred_type_argument_must_be_concrete() {
    // Inside a generic function the argument's type is a variable, with no type id to embed.
    assert_eq!(
        probe::run("f = #<'t>'t { __probe__<'int> $ }; 1").unwrap_err(),
        quiver_compiler::compiler::Error::TypeArgumentNotConcrete {
            builtin: "probe".to_string(),
        }
    );
}
