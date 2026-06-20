//! Project manifests (`quiver.toml`).
//!
//! A manifest declares, among other things, the `modules` an ordered routing table that maps
//! import names to providers. See `docs/module-resolution.md` for the full design.

use serde::Deserialize;

/// A parsed `quiver.toml`.
#[derive(Debug, Clone)]
pub struct Manifest {
    pub modules: Vec<ModuleRule>,
}

/// One rule in the `modules` routing table: an (optional) import-name prefix bound to a
/// provider. A rule with no `name` binds the root (matches every import).
#[derive(Debug, Clone)]
pub struct ModuleRule {
    /// Import-name prefix this rule claims. `None` (omitted) means the root prefix.
    pub name: Option<String>,
    pub provider: Provider,
}

/// Where a rule's modules come from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Provider {
    /// The bundled standard library.
    Std,
    /// A directory of `.qv` modules, relative to the manifest's directory.
    Path(String),
    /// A single `.qv` module, relative to the manifest's directory.
    File(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ManifestError {
    Parse(String),
    /// A rule named no provider, or named more than one.
    Provider(String),
}

impl std::fmt::Display for ManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManifestError::Parse(e) => write!(f, "invalid quiver.toml: {e}"),
            ManifestError::Provider(e) => write!(f, "invalid module rule: {e}"),
        }
    }
}

impl Default for Manifest {
    /// The default manifest: standard library only. Applies when no `quiver.toml` is present
    /// and to source with no file path (inline `--eval`, REPL, MCP tools).
    fn default() -> Self {
        Self {
            modules: vec![ModuleRule {
                name: None,
                provider: Provider::Std,
            }],
        }
    }
}

impl Manifest {
    pub fn parse(source: &str) -> Result<Self, ManifestError> {
        let raw: RawManifest =
            toml::from_str(source).map_err(|e| ManifestError::Parse(e.to_string()))?;

        // An absent `modules` key defaults to the standard library only.
        let Some(raw_rules) = raw.modules else {
            return Ok(Manifest::default());
        };

        let modules = raw_rules
            .into_iter()
            .map(ModuleRule::from_raw)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Manifest { modules })
    }
}

impl ModuleRule {
    fn from_raw(raw: RawRule) -> Result<Self, ManifestError> {
        let provider = match (raw.std, raw.path, raw.file) {
            (Some(true), None, None) => Provider::Std,
            (None | Some(false), Some(path), None) => Provider::Path(path),
            (None | Some(false), None, Some(file)) => Provider::File(file),
            (None | Some(false), None, None) => {
                return Err(ManifestError::Provider(
                    "a rule must name exactly one provider (std/path/file)".to_string(),
                ));
            }
            _ => {
                return Err(ManifestError::Provider(
                    "a rule must name exactly one provider, not several".to_string(),
                ));
            }
        };
        Ok(ModuleRule {
            name: raw.name,
            provider,
        })
    }
}

#[derive(Debug, Deserialize)]
struct RawManifest {
    modules: Option<Vec<RawRule>>,
}

#[derive(Debug, Deserialize)]
struct RawRule {
    name: Option<String>,
    std: Option<bool>,
    path: Option<String>,
    file: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absent_modules_defaults_to_std() {
        let m = Manifest::parse("[project]\nname = \"x\"\n").unwrap();
        assert_eq!(m.modules.len(), 1);
        assert_eq!(m.modules[0].provider, Provider::Std);
        assert_eq!(m.modules[0].name, None);
    }

    #[test]
    fn parses_ordered_rules() {
        let m = Manifest::parse(
            r#"
            modules = [
              { std = true },
              { path = "./src" },
              { name = "cfg", file = "./config.qv" },
              { name = "mathx", path = "./vendor/mathx/src" },
            ]
            "#,
        )
        .unwrap();
        assert_eq!(m.modules.len(), 4);
        assert_eq!(m.modules[0].provider, Provider::Std);
        assert_eq!(m.modules[1].provider, Provider::Path("./src".into()));
        assert_eq!(m.modules[2].name.as_deref(), Some("cfg"));
        assert_eq!(m.modules[2].provider, Provider::File("./config.qv".into()));
        assert_eq!(m.modules[3].name.as_deref(), Some("mathx"));
    }

    #[test]
    fn rejects_missing_provider() {
        assert!(matches!(
            Manifest::parse("modules = [ { name = \"x\" } ]"),
            Err(ManifestError::Provider(_))
        ));
    }

    #[test]
    fn rejects_multiple_providers() {
        assert!(matches!(
            Manifest::parse("modules = [ { path = \"a\", file = \"b\" } ]"),
            Err(ManifestError::Provider(_))
        ));
    }
}
