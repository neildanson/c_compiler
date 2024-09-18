use super::Instruction;

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Instruction>,
}
