use crate::{ast::{Constant, Type}, substring::Substring};

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(Substring, Type),
}
