use crate::{ast::Type, validate::type_checker::StaticInit};

use super::Instruction;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Type, String)>,
    pub global: bool,
    pub body: Option<Vec<Instruction>>,
}

#[derive(Clone, Debug)]
pub struct StaticVariable {
    pub identifier: String,
    pub global: bool,
    pub init: StaticInit,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Function),
    StaticVariable(StaticVariable),
}
