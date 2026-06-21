//! Runs the Quiver front-end (parse + typecheck) over a document and collects diagnostics.
//! Pure and synchronous — the async backend calls this off-thread.

use crate::diagnostics::{located_error_to_diagnostic, parse_error_to_diagnostic};
use crate::documents::LineIndex;
use crate::effect::NoEffect;
use crate::symbols::document_symbols;
use quiver_compiler::Compiler;
use quiver_compiler::ModuleResolver;
use quiver_compiler::compiler::ModuleCache;
use quiver_compiler::recorder::Recorder;
use quiver_core::builtins::{BuiltinRegistry, core_modules};
use quiver_core::program::Program;
use quiver_core::types::NIL;
use std::collections::HashMap;
use std::sync::OnceLock;
use tower_lsp::lsp_types::{Diagnostic, DocumentSymbol};

/// Everything the server derives from one document: diagnostics, the span→semantics index
/// and type registry (for hover/go-to-definition), the parsed AST (reused for member-level
/// navigation without re-parsing), and the symbol outline.
pub struct Analysis {
    pub diagnostics: Vec<Diagnostic>,
    pub semantics: Option<Recorder>,
    pub program: Option<Program>,
    pub ast: Option<quiver_compiler::ast::Program>,
    pub symbols: Vec<DocumentSymbol>,
}

/// The builtin type signatures, built once. Uses the no-op [`NoEffect`] so the registry
/// type-checks builtin calls without a real I/O backend.
fn builtins() -> &'static BuiltinRegistry<NoEffect> {
    static REGISTRY: OnceLock<BuiltinRegistry<NoEffect>> = OnceLock::new();
    // `core_modules` includes the IO builtins' signatures, so `%file`/`%dns` and direct
    // `__file_read__` calls type-check with no runtime backend attached.
    REGISTRY.get_or_init(|| BuiltinRegistry::with_modules(&core_modules()))
}

/// Parse and typecheck `text`, producing diagnostics, a semantic index, and symbols. Imports
/// are resolved through `resolver`, which carries the document's project context (so `%`-imports
/// of project modules resolve, and import sites carry their origin file for go-to-definition).
pub fn analyze(text: &str, index: &LineIndex, resolver: &dyn ModuleResolver) -> Analysis {
    let ast = match quiver_compiler::parse(text) {
        Ok(ast) => ast,
        Err(err) => {
            // A syntax error blocks typechecking; report it alone. With no AST, the outline and
            // member-level navigation are unavailable until the syntax error is fixed.
            return Analysis {
                diagnostics: vec![parse_error_to_diagnostic(&err, text, index)],
                semantics: None,
                program: None,
                ast: None,
                symbols: Vec::new(),
            };
        }
    };

    // Symbols come from the AST, so they survive even when typechecking fails.
    let symbols = document_symbols(&ast, text, index);

    // Keep a copy of the AST: compilation consumes it, but the backend reuses it for
    // member-level navigation (find-references from a module member's definition site)
    // without re-parsing the document.
    let retained_ast = ast.clone();

    // The program, module cache, and recorder are caller-owned: the compiler mutates them in
    // place and we keep them afterwards, so hover and go-to-definition work on whatever
    // compiled even when the file as a whole doesn't typecheck.
    let process_types = HashMap::new();
    let mut program = Program::new();
    let mut module_cache = ModuleCache::new();
    let mut recorder = Recorder::default();
    let result = Compiler::compile(
        ast,
        &HashMap::new(),
        &mut module_cache,
        resolver,
        &mut program,
        NIL,
        &process_types,
        builtins(),
        Some(&mut recorder),
    );

    let diagnostics = match &result {
        Ok(_) => Vec::new(),
        Err(located) => vec![located_error_to_diagnostic(located, text, index)],
    };
    Analysis {
        diagnostics,
        semantics: Some(recorder),
        program: Some(program),
        ast: Some(retained_ast),
        symbols,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quiver_compiler::PackageResolver;

    fn diagnostics(text: &str) -> Vec<Diagnostic> {
        analyze(text, &LineIndex::new(text), &PackageResolver::inline()).diagnostics
    }

    #[test]
    fn valid_program_has_no_diagnostics() {
        // A program that both parses and typechecks: integer add.
        assert!(diagnostics("[1, 2] ~> __add__").is_empty());
    }

    #[test]
    fn ast_is_retained_on_success_and_dropped_on_parse_error() {
        // The backend reuses the cached AST for member-level find-references; it must be present
        // whenever the document parses, and absent (no partial AST) on a syntax error.
        let ok = analyze(
            "x = 5",
            &LineIndex::new("x = 5"),
            &PackageResolver::inline(),
        );
        assert!(ok.ast.is_some(), "AST retained when the document parses");

        let bad = analyze(
            "x = [1, 2",
            &LineIndex::new("x = [1, 2"),
            &PackageResolver::inline(),
        );
        assert!(bad.ast.is_none(), "no AST when parsing fails");
    }

    #[test]
    fn syntax_error_produces_a_located_diagnostic() {
        let diags = diagnostics("x = [1, 2");
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].severity,
            Some(tower_lsp::lsp_types::DiagnosticSeverity::ERROR)
        );
    }

    #[test]
    fn type_error_is_reported_with_a_range() {
        // `__add__` expects integers; a reference to an undefined variable fails to compile.
        let diags = diagnostics("nope ~> __add__");
        assert_eq!(diags.len(), 1, "expected one diagnostic, got {diags:?}");
    }

    #[test]
    fn io_builtins_typecheck_via_their_signatures() {
        // `std/file.qv` calls `__file_read__` etc.; importing it must type-check using the IO
        // builtins' signatures, with no native io-uring backend in the language server.
        let diags = diagnostics("file = %file");
        assert!(diags.is_empty(), "expected no diagnostics, got {diags:?}");
    }

    #[test]
    fn std_imports_resolve_via_the_bundled_loader() {
        // Exercises the bundled std module loader (`%math` → std/math.qv).
        let diags = diagnostics("[1, 2] ~> %math.add");
        assert!(diags.is_empty(), "expected no diagnostics, got {diags:?}");
    }

    #[test]
    fn records_type_and_definition_for_a_reference() {
        let text = "double = #'int { ~ }\n5 ~> double";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "should typecheck: {:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics recorded");
        // The `double` *reference* is the last occurrence.
        let ref_offset = text.rfind("double").unwrap();
        let info = semantics.at_offset(ref_offset).expect("reference recorded");
        // Go-to-definition points back at the binding (the first `double`).
        let def = info.definition.expect("definition recorded");
        assert_eq!(&text[def.offset..def.offset + def.length], "double");
        assert_eq!(def.offset, 0);
        // The type formats to something non-empty (for hover).
        let program = analysis.program.unwrap();
        assert!(!quiver_core::format::format_type_by_id(&program, info.type_id).is_empty());
    }

    #[test]
    fn semantics_survive_a_type_error_elsewhere_in_the_file() {
        // `double` typechecks; the second line fails (undefined builtin). Hover and
        // go-to-definition must still work on the first line.
        let text = "double = #'int { ~ }\n5 ~> double ~> __nope__";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert_eq!(analysis.diagnostics.len(), 1, "expected the type error");
        let semantics = analysis
            .semantics
            .expect("partial semantics retained despite the error");
        let program = analysis
            .program
            .expect("partial program retained for hover");
        // The `double` reference on line 2 resolves, with a definition and a type.
        let ref_offset = text.rfind("double").unwrap();
        let info = semantics.at_offset(ref_offset).expect("reference recorded");
        assert!(info.definition.is_some(), "go-to-definition still works");
        assert!(!quiver_core::format::format_type_by_id(&program, info.type_id).is_empty());
    }

    /// Resolve the definition span of the symbol referenced at the last occurrence of
    /// `needle`, asserting it points at `expected_def` (the first occurrence by default).
    fn assert_goto(text: &str, reference: &str, expected_def: &str) {
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics recorded");
        let ref_offset = text.rfind(reference).unwrap();
        let info = semantics
            .at_offset(ref_offset)
            .unwrap_or_else(|| panic!("no semantics at reference {reference:?}"));
        let def = info
            .definition
            .unwrap_or_else(|| panic!("no definition recorded for {reference:?}"));
        let def_text = &text[def.offset..def.offset + def.length];
        assert_eq!(def_text, expected_def, "definition text for {reference:?}");
        assert_eq!(
            def.offset,
            text.find(expected_def).unwrap(),
            "definition offset for {reference:?}"
        );
    }

    #[test]
    fn goto_definition_for_tuple_destructuring_binding() {
        // `y` is bound by destructuring, then referenced; goto should land on the binding.
        let text = "[x, y] = [1, 2]\ny ~> __increment__";
        assert_goto(text, "y", "y");
    }

    #[test]
    fn goto_definition_for_named_tuple_destructuring() {
        let text = "Point[x, y] = Point[10, 20]\nx ~> __increment__";
        assert_goto(text, "x", "x");
    }

    #[test]
    fn module_member_span_locates_a_field_label() {
        // The module's value is a tuple literal; its members' labels are findable for
        // member-level go-to-definition.
        let src = "[ double: #'int { ~ }, triple: #'int { ~ } ]";
        let ast = quiver_compiler::parse(src).unwrap();
        let span = crate::symbols::module_member_span(&ast, "triple").expect("member span");
        assert_eq!(&src[span.offset..span.offset + span.length], "triple");
        assert!(crate::symbols::module_member_span(&ast, "missing").is_none());
    }

    #[test]
    fn module_member_at_finds_the_member_under_the_cursor() {
        // For find-references from a member's definition site.
        let src = "[ double: #'int { ~ }, triple: #'int { ~ } ]";
        let ast = quiver_compiler::parse(src).unwrap();
        let (name, span) = crate::symbols::module_member_at(&ast, src.find("triple").unwrap())
            .expect("member at cursor");
        assert_eq!(name, "triple");
        assert_eq!(&src[span.offset..span.offset + span.length], "triple");
        // The opening `[` is not on any member label.
        assert!(crate::symbols::module_member_at(&ast, 0).is_none());
    }

    #[test]
    fn goto_definition_for_partial_destructuring_binding() {
        // `(x)` binds the field by name (the form used by `(double) = %util`); a later use
        // should jump back to the binding.
        let text = "(x) = [x: 5]\nx ~> __increment__";
        assert_goto(text, "x", "x");
    }

    #[test]
    fn goto_definition_for_mid_chain_bind() {
        // The `=request` mid-chain binding form.
        let text = "5 ~> =request\nrequest ~> __increment__";
        assert_goto(text, "request", "request");
    }

    #[test]
    fn pattern_binding_site_is_hoverable() {
        // Hovering the binding identifier itself yields its type.
        let text = "[x, y] = [1, 2]\nx";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        // The binding `x` is the first occurrence.
        let bind_offset = text.find('x').unwrap();
        let info = semantics
            .at_offset(bind_offset)
            .expect("binding site recorded");
        assert!(!quiver_core::format::format_type_by_id(&program, info.type_id).is_empty());
    }

    #[test]
    fn hover_on_a_builtin_shows_its_signature_and_label() {
        use quiver_compiler::recorder::SymbolKind;
        let text = "[1, 2] ~> __add__";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        let offset = text.find("__add__").unwrap();
        let info = semantics.at_offset(offset).expect("builtin recorded");
        assert_eq!(info.kind, SymbolKind::Builtin);
        assert_eq!(info.label.as_deref(), Some("__add__"));
        // The recorded type is the callable signature, not the applied result.
        let sig = quiver_core::format::format_type_by_id(&program, info.type_id);
        assert!(sig.starts_with('#'), "expected a signature, got {sig:?}");
        assert!(sig.contains("->"), "expected a signature, got {sig:?}");
    }

    #[test]
    fn tail_call_hovers_and_navigates_to_the_function() {
        // `^fact` is an access whose source is the tail target; hovering it shows the function's
        // type and go-to-definition jumps to its binding.
        let text = "fact = #'int { ~ },\nrun = #'int { ~ ~> ^fact }";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        let info = semantics
            .at_offset(text.rfind("fact").unwrap())
            .expect("tail call recorded");
        // Go-to-definition points at the `fact` binding.
        assert_eq!(info.definition.expect("definition").offset, 0);
        // Hover shows the function's type.
        assert!(quiver_core::format::format_type_by_id(&program, info.type_id).contains("->"));
    }

    #[test]
    fn ripple_field_access_records_each_component() {
        use quiver_compiler::recorder::SymbolKind;
        // `~.x` hovers as two components: the `~` (the flowing value) and the `x` (the field).
        let text = "pt = [x: 5, y: 10],\npt ~> ~.x";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        let tilde = text.rfind("~.x").unwrap();
        // The `~` hovers as the flowing tuple's type.
        let base = semantics.at_offset(tilde).expect("~ recorded");
        assert!(quiver_core::format::format_type_by_id(&program, base.type_id).contains("x:"));
        // The `x` accessor hovers as the field's type, recorded as a Field.
        let field = semantics.at_offset(tilde + 2).expect("accessor recorded");
        assert_eq!(field.kind, SymbolKind::Field);
    }

    #[test]
    fn hover_on_a_builtin_reference_is_recorded() {
        use quiver_compiler::recorder::SymbolKind;
        // `&__add__` references the builtin without applying it.
        let text = "&__add__";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let offset = text.find("__add__").unwrap();
        let info = semantics.at_offset(offset).expect("builtin ref recorded");
        assert_eq!(info.kind, SymbolKind::Builtin);
        assert_eq!(info.label.as_deref(), Some("__add__"));
    }

    #[test]
    fn field_access_components_are_labelled_separately() {
        // `$.0` splits into `$` (the parameter) and `0` (the accessed field).
        let text = "sum = #['int, 'int] { $.0 }";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let base = semantics
            .at_offset(text.find("$.0").unwrap())
            .expect("parameter recorded");
        assert_eq!(base.label.as_deref(), Some("$"));
        let accessor = semantics
            .at_offset(text.find(".0").unwrap() + 1)
            .expect("accessor recorded");
        assert_eq!(accessor.label.as_deref(), Some("0"));
    }

    #[test]
    fn variable_field_access_components_are_labelled_separately() {
        // `p.x` splits into `p` (the variable, with go-to-definition) and `x` (the field).
        let text = "p = [x: 1, y: 2]\np.x";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let base = semantics
            .at_offset(text.rfind("p.x").unwrap())
            .expect("base recorded");
        assert_eq!(base.label.as_deref(), Some("p"));
        assert!(base.definition.is_some(), "base resolves to its binding");
        let accessor = semantics
            .at_offset(text.rfind(".x").unwrap() + 1)
            .expect("accessor recorded");
        assert_eq!(accessor.label.as_deref(), Some("x"));
    }

    #[test]
    fn ampersand_reference_to_a_variable_supports_hover_and_goto() {
        // `&double` references the function without calling it; hovering it should show a
        // type and go-to-definition should land on the `double` binding.
        let text = "double = #'int { ~ }\n&double";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        let ref_offset = text.rfind("double").unwrap();
        let info = semantics.at_offset(ref_offset).expect("reference recorded");
        let def = info.definition.expect("definition recorded");
        assert_eq!(def.offset, 0, "goto lands on the binding");
        assert!(!quiver_core::format::format_type_by_id(&program, info.type_id).is_empty());
    }

    #[test]
    fn ampersand_reference_to_a_module_member_is_recorded() {
        use quiver_compiler::recorder::SymbolKind;
        let text = "&%math.add";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let offset = text.find("%math.add").unwrap();
        let info = semantics.at_offset(offset).expect("import ref recorded");
        assert_eq!(info.kind, SymbolKind::Import);
        assert_eq!(info.label.as_deref(), Some("%math.add"));
        // The standard library has no openable origin, so there is nothing to jump to.
        assert_eq!(info.definition_module, None);
    }

    #[test]
    fn project_import_records_its_origin_file_for_goto_definition() {
        use quiver_compiler::PackageResolver;

        // A throwaway project: a manifest exposing `./src`, and a `util` module in it.
        let dir = std::env::temp_dir().join(format!("quiver-lsp-origin-{}", std::process::id()));
        let src = dir.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            dir.join("quiver.toml"),
            "modules = [ { std = true }, { path = \"./src\" } ]",
        )
        .unwrap();
        let util = src.join("util.qv");
        std::fs::write(&util, "[ double: #'int { %math.mul [~, 2] } ]").unwrap();

        // Analyse a document in the project that imports the project module.
        let text = "&%util.double";
        let resolver = PackageResolver::for_entry_file(&src.join("main.qv"));
        let analysis = analyze(text, &LineIndex::new(text), &resolver);
        let semantics = analysis.semantics.expect("semantics");
        let info = semantics
            .at_offset(text.find("%util").unwrap())
            .expect("import ref recorded");

        // The project module resolved (so a type was recorded), and go-to-definition points at
        // the module's own file.
        assert_eq!(
            info.definition_module,
            Some(std::fs::canonicalize(&util).unwrap())
        );

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn local_references_finds_all_uses_in_the_file() {
        // `x` is bound once and used twice.
        let text = "x = 5\n[x, x] ~> __add__";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let use_offset = text.rfind('x').unwrap();

        // From a use, without the declaration: the two uses.
        let uses = semantics.local_references(use_offset, false);
        assert_eq!(uses.len(), 2);

        // Including the declaration adds the binding site.
        let with_decl = semantics.local_references(use_offset, true);
        assert_eq!(with_decl.len(), 3);

        // Querying from the binding site itself yields the same uses.
        assert_eq!(semantics.local_references(0, false).len(), 2);
    }

    #[test]
    fn import_references_match_a_member_across_a_file() {
        use quiver_compiler::PackageResolver;

        let dir = std::env::temp_dir().join(format!("quiver-lsp-refs-{}", std::process::id()));
        let src = dir.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            dir.join("quiver.toml"),
            "modules = [ { std = true }, { path = \"./src\" } ]",
        )
        .unwrap();
        let util = src.join("util.qv");
        std::fs::write(
            &util,
            "[ double: #'int { %math.mul [~, 2] }, triple: #'int { %math.mul [~, 3] } ]",
        )
        .unwrap();

        // Two references to `%util.double` and one to `%util.triple`.
        let text = "#{ [ %util.double 1, %util.double 2, %util.triple 3 ] }";
        let resolver = PackageResolver::for_entry_file(&src.join("main.qv"));
        let analysis = analyze(text, &LineIndex::new(text), &resolver);
        let semantics = analysis.semantics.expect("semantics");

        // The cursor on the member component `double` identifies (module file, member); the
        // cursor on the `%util` base identifies the module (no member).
        let module = std::fs::canonicalize(&util).unwrap();
        let on_member = text.find(".double").unwrap() + 1;
        assert_eq!(
            semantics.import_at(on_member),
            Some((module.clone(), Some("double".to_string())))
        );
        let on_base = text.find("%util").unwrap();
        assert_eq!(semantics.import_at(on_base), Some((module.clone(), None)));

        // Member-level references find just the two `double` sites.
        assert_eq!(
            semantics.import_references(&module, Some("double")).len(),
            2
        );
        // Module-level references find each `%util` base — one per use (2 + 1).
        assert_eq!(semantics.import_references(&module, None).len(), 3);

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn destructured_import_site_is_a_reference_to_the_member() {
        use quiver_compiler::PackageResolver;

        let dir = std::env::temp_dir().join(format!("quiver-lsp-destr-{}", std::process::id()));
        let src = dir.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            dir.join("quiver.toml"),
            "modules = [ { std = true }, { path = \"./src\" } ]",
        )
        .unwrap();
        let util = src.join("util.qv");
        std::fs::write(&util, "[ double: #'int { %math.mul [~, 2] } ]").unwrap();

        // `double` is destructured (no `%util.double` access) and then used twice.
        let text = "(double) = %util,\n#{ [ double 1, double 2 ] }";
        let resolver = PackageResolver::for_entry_file(&src.join("main.qv"));
        let analysis = analyze(text, &LineIndex::new(text), &resolver);
        let semantics = analysis.semantics.expect("semantics");
        let module = std::fs::canonicalize(&util).unwrap();

        // The member's references are import-surface sites only: just the destructure `(double)`.
        // The local uses are references to the local `double`, queried separately.
        let refs = semantics.import_references(&module, Some("double"));
        assert_eq!(refs.len(), 1, "the destructure site only");

        // The `(double)` binding also identifies the member, so go-to-definition can chain onward
        // from the binding to the member's definition.
        let (m, member) = semantics
            .import_member_at(text.find("double").unwrap())
            .expect("destructure import identity");
        assert_eq!(m, module);
        assert_eq!(member, "double");

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn hover_on_a_tuple_field_label_shows_its_type() {
        // A module-shaped value: hovering a field label shows that member's type.
        let text = "[ n: 5, f: #'int { ~ } ]";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.unwrap();
        let fmt = |id| quiver_core::format::format_type_by_id(&program, id);

        let n = semantics
            .at_offset(text.find("n:").unwrap())
            .expect("field `n` recorded");
        assert_eq!(n.kind, quiver_compiler::recorder::SymbolKind::Field);
        assert_eq!(fmt(n.type_id), "'int");

        let f = semantics
            .at_offset(text.find("f:").unwrap())
            .expect("field `f` recorded");
        assert!(fmt(f.type_id).contains("->"), "function member type");
    }

    #[test]
    fn hover_on_operators_shows_their_inferred_types() {
        use quiver_compiler::recorder::SymbolKind;
        let text = "f = #'int { ~ },\nt = [a: 1, b: 2],\n5 ~> [~, 1],\np = @{ 42 },\n!p";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.unwrap();
        let at = |needle: &str, off: usize| {
            let info = semantics
                .at_offset(text.find(needle).unwrap() + off)
                .unwrap_or_else(|| panic!("no semantics at {needle:?}"));
            (
                info.kind,
                quiver_core::format::format_type_by_id(&program, info.type_id),
            )
        };
        // `#` → the inferred function type.
        let (kind, ty) = at("#", 0);
        assert_eq!(kind, SymbolKind::Expression);
        assert!(ty.contains("->"), "{ty}");
        // The tuple literal → the constructed composite type.
        assert!(at("[a:", 0).1.contains("a:"));
        // `~` (in `[~, 1]`) → the flowing value's type.
        assert_eq!(at("[~, 1]", 1).1, "'int");
        // `@` → the process type; `!` → the awaited result.
        assert!(at("@{", 0).1.contains('@'));
        assert_eq!(at("!p", 0).1, "'int");
    }

    #[test]
    fn hover_on_a_call_argument_tuple_shows_its_type() {
        use quiver_compiler::recorder::SymbolKind;
        // The `[` of a call's argument tuple hovers as that tuple's type.
        let text = "__add__ [3, 4]";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.unwrap();
        let info = semantics
            .at_offset(text.find("[3").unwrap())
            .expect("argument tuple recorded");
        assert_eq!(info.kind, SymbolKind::Expression);
        assert!(
            quiver_core::format::format_type_by_id(&program, info.type_id).contains("'int"),
            "argument tuple type"
        );
    }

    #[test]
    fn operator_hover_covers_only_the_token_not_the_interior() {
        use quiver_compiler::recorder::SymbolKind;
        let text = "f = #'int { [~, 1] ~> __add__ },\nt = [a: 1, b: 2]";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let is_expression = |offset: usize| {
            semantics
                .at_offset(offset)
                .is_some_and(|info| info.kind == SymbolKind::Expression)
        };
        // The `#` and the outer tuple's `[` carry the construct's type...
        assert!(
            is_expression(text.find('#').unwrap()),
            "`#` hovers the function"
        );
        assert!(
            is_expression(text.find("[a").unwrap()),
            "`[` hovers the tuple"
        );
        // ...but the interior does not (the `~>` inside the body, the `,` inside the tuple).
        assert!(
            !is_expression(text.find("~> __add").unwrap()),
            "fn body interior"
        );
        assert!(!is_expression(text.find(", b").unwrap()), "tuple interior");
    }

    #[test]
    fn hover_on_a_called_function_shows_its_type_not_the_result() {
        // `f 5` calls `f`; hover on `f` must show its function type, not the call's `'int`.
        let text = "f = #'int { ~ }\nf 5";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.unwrap();
        let info = semantics
            .at_offset(text.rfind('f').unwrap())
            .expect("reference recorded");
        let ty = quiver_core::format::format_type_by_id(&program, info.type_id);
        assert!(
            ty.contains("->"),
            "expected the function type for a called function, got {ty:?}"
        );
    }

    #[test]
    fn builtin_span_excludes_the_argument() {
        // Hovering inside the argument must not resolve to the builtin: the builtin's
        // recorded span covers only `__add__`, not `[1, 2]`.
        let text = "__add__ [1, 2]";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let arg_offset = text.find('1').unwrap();
        assert!(
            semantics.at_offset(arg_offset).is_none(),
            "argument position should not resolve to the builtin"
        );
    }

    #[test]
    fn hover_on_a_module_member_shows_its_signature_and_label() {
        use quiver_compiler::recorder::SymbolKind;
        let text = "[1, 2] ~> %math.add";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let semantics = analysis.semantics.expect("semantics");
        let program = analysis.program.expect("program");
        // The member component `add` hovers as its own signature.
        let offset = text.find(".add").unwrap() + 1;
        let info = semantics.at_offset(offset).expect("import member recorded");
        assert_eq!(info.kind, SymbolKind::Import);
        assert_eq!(info.label.as_deref(), Some("add"));
        // The recorded type is the member's signature, not the applied result.
        let sig = quiver_core::format::format_type_by_id(&program, info.type_id);
        assert!(sig.starts_with('#'), "expected a signature, got {sig:?}");
        assert!(sig.contains("->"), "expected a signature, got {sig:?}");
    }

    #[test]
    fn import_span_excludes_the_argument() {
        // Hovering inside the call argument must not resolve to the import member.
        let text = "%math.add [1, 2]";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let semantics = analysis.semantics.expect("semantics");
        let arg_offset = text.rfind('1').unwrap();
        assert!(
            semantics.at_offset(arg_offset).is_none(),
            "argument position should not resolve to the import"
        );
    }

    #[test]
    fn undefined_builtin_diagnostic_points_at_the_builtin() {
        // Regression: the error used to land on the `~` argument because compiling the
        // argument clobbered the located span before the builtin was resolved.
        let text = "5 ~> __nope__ [~, 1]";
        let diags = diagnostics(text);
        assert_eq!(diags.len(), 1);
        let start = diags[0].range.start;
        // Offset of `__nope__` in the source.
        let builtin_col = text.find("__nope__").unwrap() as u32;
        assert_eq!(start.line, 0);
        assert_eq!(
            start.character, builtin_col,
            "diagnostic should start at the builtin, not the argument"
        );
    }

    #[test]
    fn document_symbols_lists_bindings_and_aliases() {
        let text = "'point = Point[x: 'int]\ndouble = #'int { ~ }\nx = 5";
        let analysis = analyze(text, &LineIndex::new(text), &PackageResolver::inline());
        let names: Vec<&str> = analysis.symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"'point"), "got {names:?}");
        assert!(names.contains(&"double"), "got {names:?}");
        assert!(names.contains(&"x"), "got {names:?}");
        use tower_lsp::lsp_types::SymbolKind;
        let double = analysis
            .symbols
            .iter()
            .find(|s| s.name == "double")
            .unwrap();
        assert_eq!(double.kind, SymbolKind::FUNCTION);
    }
}
