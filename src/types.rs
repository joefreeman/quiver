use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::bytecode::TypeId;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    pub parameter: Vec<Type>,
    pub result: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Type {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "tuple")]
    Tuple(TypeId),
    #[serde(rename = "fn")]
    Function(Box<FunctionType>),
}

#[derive(Debug, Clone)]
pub struct TypeRegistry {
    types: HashMap<TypeId, (Option<String>, Vec<(Option<String>, Type)>)>,
    next_id: usize,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            types: HashMap::new(),
            next_id: 0,
        };

        let nil_type_id = registry.register_type(None, vec![]);
        assert_eq!(nil_type_id, TypeId::NIL);

        let nil_type_id = registry.register_type(Some("Ok".to_string()), vec![]);
        assert_eq!(nil_type_id, TypeId::OK);

        registry
    }

    pub fn register_type(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return existing_id;
            }
        }

        let type_id = TypeId(self.next_id);
        self.next_id += 1;

        self.types.insert(type_id, (name, fields));
        type_id
    }

    pub fn lookup_type(
        &self,
        type_id: &TypeId,
    ) -> Option<&(Option<String>, Vec<(Option<String>, Type)>)> {
        self.types.get(type_id)
    }

    pub fn get_types(&self) -> &HashMap<TypeId, (Option<String>, Vec<(Option<String>, Type)>)> {
        &self.types
    }

    pub fn find_type(
        &self,
        name: Option<String>,
        fields: &[(Option<String>, Type)],
    ) -> Option<TypeId> {
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return Some(existing_id);
            }
        }
        None
    }

    pub fn get_tuple_name(&self, type_id: &TypeId) -> String {
        if let Some((name, _)) = self.lookup_type(type_id) {
            let default_name = format!("Type{}", type_id.0);
            name.as_deref().unwrap_or(&default_name).to_string()
        } else {
            format!("Type{}", type_id.0)
        }
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        match type_def {
            Type::Integer => "int".to_string(),
            Type::Binary => "bin".to_string(),
            Type::Tuple(type_id) => {
                if let Some((name, fields)) = self.lookup_type(type_id) {
                    if fields.is_empty() {
                        name.as_deref().unwrap_or("[]").to_string()
                    } else {
                        let field_strs: Vec<String> = fields
                            .iter()
                            .map(|(field_name, field_type)| {
                                if let Some(field_name) = field_name {
                                    format!("{}: {}", field_name, self.format_type(field_type))
                                } else {
                                    self.format_type(field_type)
                                }
                            })
                            .collect();

                        if let Some(type_name) = name {
                            format!("{}[{}]", type_name, field_strs.join(", "))
                        } else {
                            format!("[{}]", field_strs.join(", "))
                        }
                    }
                } else {
                    format!("Type{}", type_id.0)
                }
            }
            Type::Function(func_type) => {
                let param_str = if func_type.parameter.len() == 1 {
                    self.format_type(&func_type.parameter[0])
                } else {
                    let params: Vec<String> = func_type
                        .parameter
                        .iter()
                        .map(|t| self.format_type(t))
                        .collect();
                    format!("({})", params.join(", "))
                };
                let result_str = if func_type.result.len() == 1 {
                    self.format_type(&func_type.result[0])
                } else {
                    let results: Vec<String> = func_type
                        .result
                        .iter()
                        .map(|t| self.format_type(t))
                        .collect();
                    format!("({})", results.join(", "))
                };
                format!("#{} -> {}", param_str, result_str)
            }
        }
    }
}
