use super::Instruction;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub global: bool,
    pub body: Option<Vec<Instruction>>,
}

#[derive(Clone, Debug)]
pub struct StaticVariable {
    pub identifier: String,
    pub global: bool,
    pub init : i32
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Function(Function),
    StaticVariable(StaticVariable),
}