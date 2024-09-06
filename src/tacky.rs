use crate::*;

#[derive(Clone, Debug)]
pub struct Program {
    pub function: Function,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub body: Vec<Instruction>,
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Return(Value),
    Unary { op: UnaryOp, src: Value, dst: Value },
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(i32),
    Var(String),
}

#[derive(Clone, Debug)]
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

#[derive(Default)]
pub struct Tacky {
    counter: u32,
}

impl Tacky {
    fn make_name(&mut self) -> String {
        let name = format!("tacky{}", self.counter);
        self.counter += 1;
        name
    }

    fn emit_tacky_expr(
        &mut self,
        e: parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        match e {
            parse::Expression::Constant(c) => Value::Constant(c),
            parse::Expression::Unary(op, inner) => {
                let src = self.emit_tacky_expr(*inner, instructions);
                let dst_name = self.make_name();
                let dst = Value::Var(dst_name);
                let tacky_op = convert_unop(op);
                instructions.push(Instruction::Unary {
                    op: tacky_op,
                    src,
                    dst: dst.clone(),
                });
                dst
            }
        }
    }

    fn emit_tacky_function(&mut self, f: parse::Function) -> Function {
        let mut body = Vec::new();
        for statement in f.body {
            match statement {
                parse::Statement::Return(expression) => {
                    let value = self.emit_tacky_expr(expression, &mut body);
                    body.push(Instruction::Return(value));
                }
            }
        }
        Function { name: f.name, body }
    }

    pub fn emit_tacky(&mut self, p: parse::Program) -> Program {
        let function = self.emit_tacky_function(p.function);
        Program { function }
    }
}
