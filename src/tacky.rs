use crate::*;

#[derive(Clone)]
pub struct Program {
    function: Function,
}

#[derive(Clone)]
pub struct Function {
    identifier: String,
    body: Vec<Instruction>,
}

#[derive(Clone)]
pub enum Instruction {
    Return(Value),
    Unary { op: UnaryOp, src: Value, dst: Value },
}

#[derive(Clone)]
pub enum Value {
    Constant(i32),
    Var(String),
}

#[derive(Clone)]
pub enum UnaryOp {
    Complement,
    Negate,
}

fn convert_unop(op: parse::UnaryOperator) -> UnaryOp {
    match op {
        parse::UnaryOperator::Negation => UnaryOp::Negate,
        parse::UnaryOperator::Tilde => UnaryOp::Complement,
    }
}

pub struct Tacky {
    counter: u32,
}

impl Tacky {
    pub fn new() -> Self {
        Tacky { counter: 0 }
    }

    fn make_name(&mut self) -> String {
        let name = format!("tacky{}", self.counter);
        self.counter += 1;
        name
    }

    pub fn emit_tacky(&mut self, e: parse::Expression, instructions: &mut Vec<Instruction>) -> Value {
        match e {
            parse::Expression::Constant(c) => {
                return Value::Constant(c);
            }
            parse::Expression::Unary(op, inner) => {
                let src = self.emit_tacky(*inner, instructions);
                let dst_name = self.make_name();
                let dst = Value::Var(dst_name);
                let tacky_op = convert_unop(op);
                instructions.push(Instruction::Unary {
                    op: tacky_op,
                    src,
                    dst: dst.clone(),
                });
                return dst;
            }
        }
    }
}
