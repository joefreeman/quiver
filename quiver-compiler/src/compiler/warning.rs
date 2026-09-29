//! Compiler warnings: code that compiles, but is probably not what its author meant.

use super::ModuleSite;
use crate::parser::SourceSpan;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Warning {
    /// A branch no input can reach: the branch before it can't fail, so the block never gets
    /// past it.
    UnreachableBranch {
        /// The branch that can't fail.
        cause: SourceSpan,
    },
    /// A step with a match that can never succeed — its pattern can't match any value of the
    /// type it is given — so the step always fails, and nothing after it runs.
    ImpossibleMatch,
    /// A step that is always nil, so it always fails, and the steps after it never run.
    AlwaysNil,
    /// A binding nothing reads.
    UnusedBinding { name: String },
    /// A function whose parameter type no value has (`#('int & 'bin)`), so it can never be
    /// called.
    UninhabitedParameter,
}

impl Warning {
    /// What to say at the warning's own position.
    pub fn label(&self) -> &'static str {
        match self {
            Warning::UnreachableBranch { .. } => "this branch never runs",
            Warning::ImpossibleMatch => "this step never succeeds",
            Warning::AlwaysNil => "this step is always nil",
            Warning::UnusedBinding { .. } => "never read",
            Warning::UninhabitedParameter => "no value has this function's parameter type",
        }
    }

    /// How to act on the warning, where that isn't obvious from it.
    pub fn help(&self) -> Option<&'static str> {
        match self {
            Warning::UnusedBinding { .. } => {
                Some("to match without binding, write `_` in place of the name")
            }
            _ => None,
        }
    }

    /// Further positions worth pointing at, in the same source as the warning, each with
    /// what to say about it.
    pub fn related(&self) -> Vec<(SourceSpan, &'static str)> {
        match self {
            Warning::UnreachableBranch { cause } => vec![(*cause, "this branch can't fail")],
            _ => Vec::new(),
        }
    }
}

impl std::fmt::Display for Warning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Warning::UnreachableBranch { .. } => write!(
                f,
                "Unreachable branch: the branch before it can't fail, so the block never gets \
                 past it"
            ),
            Warning::ImpossibleMatch => write!(
                f,
                "This step always fails: a match in it can never succeed, as its pattern \
                 can't match the type of value it is given"
            ),
            Warning::AlwaysNil => write!(
                f,
                "This step always fails: it is always nil, so the steps after it never run"
            ),
            Warning::UnusedBinding { name } => write!(f, "Unused binding: nothing reads `{name}`"),
            Warning::UninhabitedParameter => write!(
                f,
                "Uninhabited parameter: no value has this function's parameter type, so the \
                 function can never be called"
            ),
        }
    }
}

/// A [`Warning`] and where it is: in the compiled source, or in an imported module.
#[derive(Debug, Clone, PartialEq)]
pub struct LocatedWarning {
    pub warning: Warning,
    /// The position in the compiled source, for a warning there.
    pub span: Option<SourceSpan>,
    /// For a warning in an imported module: that module, with the warning's position in it.
    /// Warnings don't say which import reached a module — a module's warnings are its own,
    /// reported once whoever imports it.
    pub module: Option<ModuleSite>,
}

impl std::fmt::Display for LocatedWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.warning)?;
        if let Some(site) = &self.module {
            match site.span {
                Some(span) => write!(f, "\n  at {}:{}:{}", site.name, span.line, span.column)?,
                None => write!(f, "\n  in {}", site.name)?,
            }
        }
        Ok(())
    }
}
