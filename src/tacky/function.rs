use super::Instruction;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Vec<Instruction>>,
}
