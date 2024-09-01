use std::{collections::HashMap, fmt::Display};

use oxc_ast::ast::TSAccessibility;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Function(FunctionType),
    Literal(LiteralType),
    Keyword(KeywordType),
    Array(Box<Type>),
    Object(ObjectType),
    Union(Vec<Type>),
    Tuple(TupleType),

    Reference(ReferenceType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceType {}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub members: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    pub properties: HashMap<String, Type>,
    pub index_signatures: Vec<IndexSignature>,
    pub call_signatures: Vec<FunctionType>,
    pub construct_signatures: Vec<FunctionType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub readonly: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KeywordType {
    Any,
    Unknown,
    Number,
    Object,
    Boolean,
    BigInt,
    String,
    Symbol,
    Void,
    Undefined,
    Null,
    Never,
    Intrinsic,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Boolean(bool),
    Null,
    Number(f64),
    BigInt,
    RegExp,
    String,
}

impl std::fmt::Display for KeywordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeywordType::Any => write!(f, "any"),
            KeywordType::Unknown => write!(f, "unknown"),
            KeywordType::Number => write!(f, "number"),
            KeywordType::Object => write!(f, "object"),
            KeywordType::Boolean => write!(f, "boolean"),
            KeywordType::BigInt => write!(f, "bigint"),
            KeywordType::String => write!(f, "string"),
            KeywordType::Symbol => write!(f, "symbol"),
            KeywordType::Void => write!(f, "void"),
            KeywordType::Undefined => write!(f, "undefined"),
            KeywordType::Null => write!(f, "null"),
            KeywordType::Never => write!(f, "never"),
            KeywordType::Intrinsic => write!(f, "intrinsic"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub params: Vec<FunctionParameterType>,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameterType {
    pub r#type: Box<Type>,
    pub accessibility: Option<TSAccessibility>,
    pub readonly: bool,
    pub is_override: bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Union(ty) => {
                let types: Vec<String> = ty.iter().map(|t| format!("{t}")).collect();
                write!(f, "{}", types.join(" | "))
            }
            _ => {
                // default
                write!(f, "{self:?}",)
            }
        }
    }
}
