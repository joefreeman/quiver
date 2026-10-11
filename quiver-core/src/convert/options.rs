//! A conversion's options — `keys:`, `tag:` and `shapes:` — as the planner reads them: from their
//! types, when the instantiation is registered.
//!
//! What the types say is planned: a key convention (`Camel`), a tag's kind (`Internal[key: …]`),
//! which tuples `shapes:` configures and which of their fields it renames or defaults. What only
//! the values say — a renamed key, a default, a member's wire name, a tag's key — the plan points
//! at by an [`OptionPath`], and the conversion reads it from its argument. Written as literals,
//! options have types precise enough for the planner to check them where they are written: a
//! field named that the type doesn't have, a default its field can't hold.

use serde::{Deserialize, Serialize};

use crate::program::Program;
use crate::types::{NIL, TupleField, Type, TypeLookup};

/// Where an option's value lies in the conversion's argument: under the option's label, then
/// fields by label or position.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OptionPath {
    pub option: String,
    pub steps: Vec<PathStep>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PathStep {
    Label(String),
    Index(usize),
}

impl OptionPath {
    fn at(&self, step: PathStep) -> OptionPath {
        let mut steps = self.steps.clone();
        steps.push(step);
        OptionPath {
            option: self.option.clone(),
            steps,
        }
    }
}

/// Text a conversion writes or expects: known when the conversion is planned (a label in a
/// convention, a member's own name), or an option's value, read when it runs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Text {
    Fixed(String),
    Given(OptionPath),
}

impl Text {
    pub fn fixed(&self) -> Option<&str> {
        match self {
            Text::Fixed(text) => Some(text),
            Text::Given(_) => None,
        }
    }
}

/// A convention for writing a label as a key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Exact,
    Camel,
    Pascal,
    Kebab,
    Snake,
}

impl Style {
    fn named(name: &str) -> Option<Style> {
        Some(match name {
            "Exact" => Style::Exact,
            "Camel" => Style::Camel,
            "Pascal" => Style::Pascal,
            "Kebab" => Style::Kebab,
            "Snake" => Style::Snake,
            _ => return None,
        })
    }

    /// The key `label` is written as: itself (`Exact`), or split into words at its underscores,
    /// less a trailing `?` or `!`, as `userId` (`Camel`), `UserId` (`Pascal`), `user-id`
    /// (`Kebab`) or `user_id` (`Snake`).
    pub fn key(self, label: &str) -> String {
        if self == Style::Exact {
            return label.to_string();
        }
        let base = label.strip_suffix('!').unwrap_or(label);
        let base = base.strip_suffix('?').unwrap_or(base);
        let words: Vec<&str> = base.split('_').filter(|word| !word.is_empty()).collect();
        let capitalised = |word: &str| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
                None => String::new(),
            }
        };
        match self {
            Style::Exact => unreachable!("written as is"),
            Style::Kebab => words.join("-"),
            Style::Snake => words.join("_"),
            Style::Pascal => words.into_iter().map(capitalised).collect(),
            Style::Camel => {
                let pascal: String = words.into_iter().map(capitalised).collect();
                let mut chars = pascal.chars();
                match chars.next() {
                    Some(first) => first.to_ascii_lowercase().to_string() + chars.as_str(),
                    None => String::new(),
                }
            }
        }
    }
}

/// How the members of a union are told apart.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tag {
    /// By shape alone, each member tried in turn.
    Untagged,
    /// A record of one entry, the member's name its key: `{"Circle": {…}}`.
    External,
    /// An entry beside the member's own: `{"type": "Circle", …}`.
    Internal { key: OptionPath },
    /// Two entries, the name and the content: `{"t": "Circle", "c": {…}}`.
    Adjacent {
        tag: OptionPath,
        content: OptionPath,
    },
}

/// What `shapes:` says of one tuple, by its name.
#[derive(Debug, Clone)]
pub struct Shape {
    pub name: String,
    pub keys: Option<Style>,
    /// Each renamed field's label, and where its key is given.
    pub rename: Vec<(String, OptionPath)>,
    /// Each defaulted field's label, where its default is given, and the default's type.
    pub default: Vec<(String, OptionPath, usize)>,
    pub wire: Option<OptionPath>,
    pub tag: Option<Tag>,
}

/// A conversion's options.
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub keys: Option<Style>,
    pub tag: Option<Tag>,
    pub shapes: Vec<Shape>,
}

impl Options {
    /// The options whose types are `keys`, `tag` and `shapes`, or why they aren't options.
    pub fn read(keys: usize, tag: usize, shapes: usize, program: &Program) -> Result<Self, String> {
        let reader = Reader { program };
        Ok(Options {
            keys: reader.style(keys, "keys")?,
            tag: reader.tag(
                tag,
                &OptionPath {
                    option: "tag".to_string(),
                    steps: vec![],
                },
            )?,
            shapes: reader.shapes(shapes)?,
        })
    }

    pub fn shape(&self, name: Option<&str>) -> Option<(usize, &Shape)> {
        let name = name?;
        self.shapes
            .iter()
            .enumerate()
            .find(|(_, shape)| shape.name == name)
    }
}

struct Reader<'a> {
    program: &'a Program,
}

impl Reader<'_> {
    fn lookup(&self, type_id: usize) -> Type {
        let type_id = Type::strip_annotations(type_id, self.program);
        self.program
            .lookup_type(type_id)
            .cloned()
            .expect("an option's type is in the table")
    }

    fn describe(&self, type_id: usize) -> String {
        crate::format::format_type_by_id(self.program, type_id)
    }

    /// A tuple type's name and fields, when the type is one tuple.
    fn tuple(&self, type_id: usize) -> Option<(Option<String>, Vec<TupleField>)> {
        match self.lookup(type_id) {
            Type::Tuple(tuple) => self
                .program
                .lookup_tuple(tuple)
                .map(|info| (info.name.clone(), info.fields.clone())),
            _ => None,
        }
    }

    fn is_nil(&self, type_id: usize) -> bool {
        matches!(self.lookup(type_id), Type::Tuple(NIL))
    }

    fn is_text(&self, type_id: usize) -> bool {
        matches!(
            self.tuple(type_id),
            Some((Some(name), fields)) if name == "Str"
                && matches!(fields[..], [(None, bytes)] if matches!(self.lookup(bytes), Type::Binary))
        )
    }

    fn style(&self, type_id: usize, option: &str) -> Result<Option<Style>, String> {
        if self.is_nil(type_id) {
            return Ok(None);
        }
        match self.tuple(type_id) {
            Some((Some(name), fields)) if fields.is_empty() => Style::named(&name).map(Some),
            _ => None,
        }
        .ok_or_else(|| {
            format!(
                "`{option}:` takes Exact, Camel, Pascal, Kebab or Snake, written where the \
                 conversion is, but was given {}",
                self.describe(type_id)
            )
        })
    }

    fn tag(&self, type_id: usize, at: &OptionPath) -> Result<Option<Tag>, String> {
        if self.is_nil(type_id) {
            return Ok(None);
        }
        let text = |label: &str, fields: &[(Option<String>, usize)]| {
            fields
                .iter()
                .find(|(field, _)| field.as_deref() == Some(label))
                .filter(|&&(_, field)| self.is_text(field))
                .map(|_| at.at(PathStep::Label(label.to_string())))
        };
        let tag = match self.tuple(type_id) {
            Some((Some(name), fields)) => match (name.as_str(), fields.len()) {
                ("Untagged", 0) => Some(Tag::Untagged),
                ("External", 0) => Some(Tag::External),
                ("Internal", 1) => text("key", &fields).map(|key| Tag::Internal { key }),
                ("Adjacent", 2) => text("tag", &fields)
                    .zip(text("content", &fields))
                    .map(|(tag, content)| Tag::Adjacent { tag, content }),
                _ => None,
            },
            _ => None,
        };
        tag.map(Some).ok_or_else(|| {
            format!(
                "`tag:` takes Untagged, External, Internal[key: \"…\"] or \
                 Adjacent[tag: \"…\", content: \"…\"], but was given {}",
                self.describe(type_id)
            )
        })
    }

    fn shapes(&self, type_id: usize) -> Result<Vec<Shape>, String> {
        if self.is_nil(type_id) {
            return Ok(vec![]);
        }
        let entries = match self.tuple(type_id) {
            Some((None, entries)) if entries.iter().all(|(label, _)| label.is_none()) => entries,
            _ => {
                return Err(format!(
                    "`shapes:` takes a tuple of the tuples it configures, each named as the tuple \
                     is — [User[keys: Camel], …] — but was given {}",
                    self.describe(type_id)
                ));
            }
        };
        let mut shapes: Vec<Shape> = Vec::with_capacity(entries.len());
        for (index, &(_, entry)) in entries.iter().enumerate() {
            let at = OptionPath {
                option: "shapes".to_string(),
                steps: vec![PathStep::Index(index)],
            };
            let shape = self.shape(entry, &at)?;
            if shapes.iter().any(|known| known.name == shape.name) {
                return Err(format!("`shapes:` configures {} twice", shape.name));
            }
            shapes.push(shape);
        }
        Ok(shapes)
    }

    fn shape(&self, type_id: usize, at: &OptionPath) -> Result<Shape, String> {
        let Some((Some(name), fields)) = self.tuple(type_id) else {
            return Err(format!(
                "`shapes:` names each tuple it configures — User[keys: Camel] — but was given {}",
                self.describe(type_id)
            ));
        };
        let mut shape = Shape {
            name: name.clone(),
            keys: None,
            rename: vec![],
            default: vec![],
            wire: None,
            tag: None,
        };
        for (label, field) in fields {
            let Some(label) = label else {
                return Err(format!(
                    "`shapes:` configures {name} by label — {name}[keys: …, rename: […], \
                     default: […], wire: \"…\", tag: …]"
                ));
            };
            let field_at = at.at(PathStep::Label(label.clone()));
            match label.as_str() {
                "keys" => shape.keys = self.style(field, "keys")?,
                "tag" => shape.tag = self.tag(field, &field_at)?,
                "wire" => {
                    if !self.is_text(field) {
                        return Err(format!(
                            "`shapes:` takes {name}'s wire name as text, but was given {}",
                            self.describe(field)
                        ));
                    }
                    shape.wire = Some(field_at);
                }
                "rename" | "default" => {
                    let entries = match self.tuple(field) {
                        Some((None, entries))
                            if entries.iter().all(|(label, _)| label.is_some()) =>
                        {
                            entries
                        }
                        _ => {
                            return Err(format!(
                                "`shapes:` takes {name}'s `{label}` by field label — \
                                 {label}: [field: …] — but was given {}",
                                self.describe(field)
                            ));
                        }
                    };
                    for (field_label, value) in entries {
                        let field_label = field_label.expect("checked labelled");
                        let value_at = field_at.at(PathStep::Label(field_label.clone()));
                        if label == "rename" {
                            if !self.is_text(value) {
                                return Err(format!(
                                    "`shapes:` renames {name}'s `{field_label}` to text, but was \
                                     given {}",
                                    self.describe(value)
                                ));
                            }
                            shape.rename.push((field_label, value_at));
                        } else {
                            shape.default.push((field_label, value_at, value));
                        }
                    }
                }
                _ => {
                    return Err(format!(
                        "`shapes:` configures {name} by keys, rename, default, wire and tag, and \
                         has no `{label}`"
                    ));
                }
            }
        }
        Ok(shape)
    }
}
