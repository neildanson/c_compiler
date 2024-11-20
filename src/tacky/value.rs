use crate::ast::{Constant, Type};

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(String, Type),
}
