use crate::parse::*;

#[derive(PartialEq, Debug, Clone)]
pub enum TypeDefinition {
    Type(Type),
    FunType(Vec<Type>, Type),
}

impl TypeDefinition {
    pub fn get_type(&self) -> Type {
        match self {
            TypeDefinition::Type(ty) => ty.clone(),
            TypeDefinition::FunType(_, ty) => ty.clone(),
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, TypeDefinition::FunType(_, _))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    pub type_definition: TypeDefinition,
    pub attributes: IdentifierAttributes,
}

impl Symbol {
    pub fn new(type_definition: TypeDefinition, attributes: IdentifierAttributes) -> Self {
        Symbol {
            type_definition,
            attributes,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64),
}

impl StaticInit {
    //TODO: Remove this functionn
    pub fn i32(&self) -> i32 {
        match self {
            StaticInit::IntInit(val) => *val,
            _ => panic!("Invalid conversion"),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

impl InitialValue {
    pub fn is_constant(&self) -> bool {
        matches!(self, InitialValue::Initial(_))
    }
    pub fn is_tentative(&self) -> bool {
        matches!(self, InitialValue::Tentative)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunAttr {
    pub defined: bool,
    pub global: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StaticAttr {
    pub init: InitialValue,
    pub global: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub enum IdentifierAttributes {
    Fun(FunAttr),
    Static(StaticAttr),
    Local,
}

impl IdentifierAttributes {
    pub fn is_global(&self) -> bool {
        match self {
            IdentifierAttributes::Fun(fun_attr) => fun_attr.global,
            IdentifierAttributes::Static(static_attr) => static_attr.global,
            IdentifierAttributes::Local => false,
        }
    }

    pub fn init(&self) -> InitialValue {
        match self {
            IdentifierAttributes::Static(static_attr) => static_attr.init.clone(),
            _ => InitialValue::NoInitializer,
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, IdentifierAttributes::Static { .. })
    }
}