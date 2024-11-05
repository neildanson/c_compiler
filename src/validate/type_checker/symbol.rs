use crate::parse::*;

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

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Static(StaticAttr, Type),
    Local(Type),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Symbol {
    Value(Value),
    FunType(FunAttr, Vec<Type>, Type),
}

impl Symbol {
    pub fn is_global(&self) -> bool {
        match self {
            Symbol::FunType(fun_attr, _, _) => fun_attr.global,
            Symbol::Value(Value::Static(static_attr, _)) => static_attr.global,
            Symbol::Value(Value::Local(_)) => false,
        }
    }

    pub fn init(&self) -> InitialValue {
        match self {
            Symbol::Value(Value::Static(static_attr, _)) => static_attr.init.clone(),
            _ => InitialValue::NoInitializer,
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, Symbol::Value(Value::Static { .. }))
    }
    pub fn get_type(&self) -> Type {
        match self {
            Symbol::Value(Value::Static(_, ty)) => ty.clone(),
            Symbol::Value(Value::Local(ty)) => ty.clone(),
            Symbol::FunType(_, _, ty) => ty.clone(),
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Symbol::FunType(_, _, _))
    }
}

//Thoughts
//Move fun_attr to TypeDefinition::FunType
//Move static_attr to TypeDefinition::Type
//Move IdentifierAttributes::Local to TypeDefinition::Type ???
