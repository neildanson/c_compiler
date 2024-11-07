use crate::parse::Constant;

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Var(String),
}
