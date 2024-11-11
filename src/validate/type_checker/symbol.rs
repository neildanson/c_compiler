use std::fmt::{Display, Formatter};

use crate::parse::*;

use super::TCExpression;

#[derive(PartialEq, Clone, Debug)]
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64),
}

impl StaticInit {
    pub fn get_type(&self) -> Type {
        match self {
            StaticInit::IntInit(_) => Type::Int,
            StaticInit::LongInit(_) => Type::Long,
        }
    }

    //TODO: Remove this functionn
    pub fn i32(&self) -> i32 {
        match self {
            StaticInit::IntInit(val) => *val,
            _ => panic!("Invalid conversion"),
        }
    }
}

impl Display for StaticInit {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            StaticInit::IntInit(val) => write!(f, "{}", val),
            StaticInit::LongInit(val) => write!(f, "{}", val),
        }
    }
}

impl From<Constant> for StaticInit {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Int(val) => StaticInit::IntInit(val),
            Constant::Long(val) => StaticInit::LongInit(val),
        }
    }
}

impl From<StaticInit> for Constant {
    fn from(value: StaticInit) -> Self {
        match value {
            StaticInit::IntInit(val) => Constant::Int(val),
            StaticInit::LongInit(val) => Constant::Long(val),
        }
    }
}

impl From<StaticInit> for TCExpression {
    fn from(value: StaticInit) -> Self {
        match value {
            StaticInit::IntInit(val) => TCExpression::Constant(Constant::Int(val)),
            StaticInit::LongInit(val) => TCExpression::Constant(Constant::Long(val)),
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

    pub fn get_type(&self, fallback: Type) -> Type {
        match self {
            InitialValue::Initial(s) => s.get_type(),
            _ => fallback,
        }
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
