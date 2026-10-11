//! Expansion: running a dialect's function at compile time on the context its invocation gives
//! it, and splicing the `'%meta.expr` it returns in place of the invocation (the splice is
//! [`dialect::Splicer`]'s).
//!
//! A dialect is `%mod{ … }`, the function its module's `:dialect` annotation carries, given the
//! brace content. The context crosses into the compile-time executor as a value, so none of it
//! ships with the program, and a failure is a nil whose `:error` payload is a `'%meta.error`.

use super::dialect::{self, Content, Expander, Splicer};
use super::{Compiler, Error, ImportedMember, annotations, code, values};
use crate::ast;
use crate::parser::SourceSpan;
use quiver_core::types::{Type, TypeLookup};
use quiver_core::value::Value;

impl<E: quiver_core::effects::Effect> Compiler<'_, E> {
    /// Expand a dialect invocation `%mod{ … }`: run the function the module's `:dialect`
    /// annotation carries on the context — the unescaped brace content and the host's
    /// prefix-parsers, and the content as `code` for a function that names it — and splice the
    /// `'%meta.expr` it returns, in a block binding the flowing value (for `Ripple`) and
    /// evaluating the `Unquote` holes once.
    ///
    /// TODO: cache expansions keyed by (module, content).
    pub(super) fn expand_dialect(&mut self, invocation: &ast::Dialect) -> Result<ast::Term, Error> {
        let (content, escapes) = dialect::unescape_content(&invocation.raw);
        let ImportedMember {
            value: module_value,
            package,
            ..
        } = self.resolve_import(&invocation.path, None, &[])?;
        // Compiling the module (on a cache miss) leaves `current_span` pointing into the
        // module's source; point it back at the invocation for expansion errors.
        self.current_span = invocation.span.get();
        let expander = Expander {
            name: format!("%{}", invocation.path.join("/")),
            package,
            span: invocation.span,
        };

        let dialect_key = self.program.register_annotation_key("dialect");
        let Some(function) = module_value.get_annotation(dialect_key).cloned() else {
            return Err(Error::DialectMissing {
                module: expander.name,
            });
        };

        let registry = self.expander_registry();
        let term = self
            .program
            .register_builtin(dialect::TERM_CALLBACK.to_string(), &registry);
        let chain = self
            .program
            .register_builtin(dialect::CHAIN_CALLBACK.to_string(), &registry);
        let content_value = {
            let bytes = values::binary(self.program, content.as_bytes());
            values::node(self.program, "Str", vec![(None, bytes)])
        };
        let term = (
            Value::builtin(term),
            self.value_type(&Value::builtin(term))?,
        );
        let chain = (
            Value::builtin(chain),
            self.value_type(&Value::builtin(chain))?,
        );
        let content = Content {
            dialect: invocation,
            text: &content,
            escapes: &escapes,
        };
        let mut fields = vec![
            (Some("content".to_string()), content_value),
            (Some("term".to_string()), term),
            (Some("chain".to_string()), chain),
        ];
        // The content is parsed as Quiver only for a dialect that asks for it by naming `code`:
        // most dialects' content is a grammar of their own.
        if self.takes_field(&function, "code")? {
            let code = self.content_code(&expander, &content)?;
            fields.push((Some("code".to_string()), code));
        }

        let context = values::tuple(self.program, None, fields);
        let expansion = self.run_expander(&expander, &function, context, &registry, &content)?;
        let locals = dialect::local_prefix(self.expansions);
        self.expansions += 1;
        let splicer = Splicer::new(&*self.program, &expander, content, locals);
        let chain = splicer.value_to_chain(&expansion)?;
        Ok(dialect::wrap_expansion(splicer.finish(), chain))
    }

    /// The host's builtins, and the callbacks only a dialect is given: the prefix parsers.
    /// Expansion — and only expansion — reaches the compiler's parser this way.
    fn expander_registry(&self) -> quiver_core::builtins::BuiltinRegistry<E> {
        let mut registry = self.builtins.clone();
        dialect::register_callbacks(&mut registry);
        registry
    }

    /// Whether `function`'s parameter names the field `label`: a context field it asks for.
    fn takes_field(&mut self, function: &Value, label: &str) -> Result<bool, Error> {
        let function_type = self.value_type(function)?;
        let Some((parameter, _)) = annotations::single_callable(self.program, function_type) else {
            return Ok(false);
        };
        let parameter = Type::strip_annotations(parameter, &*self.program);
        Ok(match self.program.lookup_type(parameter) {
            Some(Type::Tuple(tuple)) => self
                .program
                .lookup_tuple(*tuple)
                .is_some_and(|info| info.fields.iter().any(|(l, _)| l.as_deref() == Some(label))),
            Some(Type::Partial { fields, .. }) => fields.iter().any(|(l, _)| l == label),
            _ => false,
        })
    }

    /// A dialect's content as Quiver code, its `'%meta.expr` (see [`code`]). Content that
    /// doesn't parse, or that the IR can't express, is an error at its position.
    fn content_code(
        &mut self,
        expander: &Expander,
        content: &Content,
    ) -> Result<(Value, usize), Error> {
        let positioned = |message: String, span: Option<SourceSpan>| {
            let error = expander.error(&message);
            match span.and_then(|span| {
                dialect::content_span(content.dialect, content.escapes, span.offset, span.length)
            }) {
                Some(span) => Error::Positioned {
                    span,
                    error: Box::new(error),
                },
                None => error,
            }
        };
        let sequence = crate::parser::parse_quoted(content.text).map_err(|error| {
            positioned(
                format!("is given code that doesn't parse: {}", error.kind),
                error.span,
            )
        })?;
        code::lower(&sequence, self.program).map_err(|error| positioned(error.message, error.span))
    }

    /// Run a dialect's function on its context at compile time, answering the `'%meta.expr` it
    /// returns. The context crosses in as a value, so nothing of it is added to the program. A
    /// nil answer fails the expansion, its `:error` payload — a `'%meta.error` — saying why, at
    /// the content position it names.
    fn run_expander(
        &mut self,
        expander: &Expander,
        function: &Value,
        (context, context_type): (Value, usize),
        registry: &quiver_core::builtins::BuiltinRegistry<E>,
        content: &Content,
    ) -> Result<Value, Error> {
        let function_type = self.value_type(function)?;
        let Some((parameter, _)) = annotations::single_callable(self.program, function_type) else {
            return Err(expander.error(&format!(
                "must give a function to run, found {}",
                quiver_core::format::format_type_by_id(&*self.program, function_type)
            )));
        };
        let parameter = Type::strip_annotations(parameter, &*self.program);
        if !quiver_core::types::is_compatible(context_type, parameter, &*self.program) {
            return Err(expander.error(&format!(
                "must take the context it is given ({}), but takes {}",
                quiver_core::format::format_type_by_id(&*self.program, context_type),
                quiver_core::format::format_type_by_id(&*self.program, parameter)
            )));
        }
        // The function is the program's own, so the executor runs over the program's tables.
        self.intern_value(function)?;
        let bytecode = self.program.to_bytecode(None);
        let (expansion, _executor) = quiver_core::call_sync(
            bytecode,
            registry,
            self.fuel,
            self.cancel.as_deref(),
            function,
            &context,
        )
        .map_err(|error| Error::DialectEvaluationFailed {
            module: expander.name.clone(),
            error: Box::new(error),
        })?;
        if !expansion.is_nil() {
            return Ok(expansion);
        }
        let error_key = self.program.register_annotation_key("error");
        let (message, span) = match expansion.get_annotation(error_key) {
            Some(payload) => self.describe_failure(payload, content),
            None => ("failed".to_string(), None),
        };
        let error = expander.error(&message);
        Err(match span {
            Some(span) => Error::Positioned {
                span,
                error: Box::new(error),
            },
            None => error,
        })
    }

    /// A dialect's `'%meta.error` as a message: a string; `Expected[offset, message]`, what was
    /// expected at a content offset; or a tuple's `message`, with its `offset` when it gives one.
    /// The offset maps to a source position through the invocation's content.
    fn describe_failure(&self, payload: &Value, content: &Content) -> (String, Option<SourceSpan>) {
        let program = &*self.program;
        let text = |value: &Value| {
            dialect::str_bytes(program, value)
                .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        };
        if let Some(message) = text(payload) {
            return (format!("failed: {message}"), None);
        }
        // `%parse`'s failure, `Expected[offset, message]`, may be built positionally, as any
        // node can; any other tuple gives its `message` (and `offset`) by label.
        let expected = matches!(payload, Value::Tuple(id, _)
            if program.lookup_tuple(*id).is_some_and(|info| info.name.as_deref() == Some("Expected")));
        let field = |label: &str, position: usize| {
            dialect::node_field(program, payload, &[label], expected.then_some(position))
        };
        if let Some(message) = field("message", 1).and_then(text) {
            let offset = field("offset", 0).and_then(|offset| match offset {
                Value::Int(offset) => usize::try_from(*offset).ok(),
                _ => None,
            });
            let span = offset.and_then(|offset| {
                dialect::content_span(content.dialect, content.escapes, offset, 1)
            });
            let message = if expected {
                format!("expected {message}")
            } else {
                message
            };
            return (format!("failed: {message}"), span);
        }
        (
            format!("failed with {}", dialect::format_value(payload, program)),
            None,
        )
    }
}
