use std::fmt::Display;

use crate::ast::{Constant, Type};

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(String, Type),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Var(name, ty) => write!(f, "{}: {}", name, ty),
        }
    }
}