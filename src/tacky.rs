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
    Unary {
        op: UnaryOp,
        src: Value,
        dst: Value,
    },
    Binary {
        op: BinaryOp,
        src1: Value,
        src2: Value,
        dst: Value,
    },
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

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

fn convert_unop(op: &parse::UnaryOperator) -> UnaryOp {
    match op {
        parse::UnaryOperator::Negation => UnaryOp::Negate,
        parse::UnaryOperator::Tilde => UnaryOp::Complement,
    }
}

fn convert_binop(op: &parse::BinaryOperator) -> BinaryOp {
    match op {
        parse::BinaryOperator::Add => BinaryOp::Add,
        parse::BinaryOperator::Sub => BinaryOp::Subtract,
        parse::BinaryOperator::Mul => BinaryOp::Multiply,
        parse::BinaryOperator::Div => BinaryOp::Divide,
        parse::BinaryOperator::Mod => BinaryOp::Remainder,
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
        e: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        match e {
            parse::Expression::Factor(f) => self.emit_tacky_factor(f, instructions),
            parse::Expression::BinOp(op, e1, e2) => self.emit_tacky_binop(op, e1, e2, instructions),
        }
    }

    fn emit_tacky_factor(
        &mut self,
        f: &parse::Factor,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        match f {
            parse::Factor::Int(i) => Value::Constant(*i),
            parse::Factor::Unary(op, inner) => self.emit_tacky_unaryop(op, &inner, instructions),
            parse::Factor::Expression(e) => self.emit_tacky_expr(e, instructions),
        }
    }

    fn emit_tacky_unaryop(
        &mut self,
        op: &parse::UnaryOperator,
        inner: &parse::Factor,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        let src = self.emit_tacky_factor(&inner, instructions);
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

    fn emit_tacky_binop(
        &mut self,
        op: &parse::BinaryOperator,
        e1: &parse::Expression,
        e2: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Value {
        let src1 = self.emit_tacky_expr(e1, instructions);
        let src2 = self.emit_tacky_expr(e2, instructions);
        let dst_name = self.make_name();
        let dst = Value::Var(dst_name);
        let tacky_op = convert_binop(op);
        instructions.push(Instruction::Binary {
            op: tacky_op,
            src1,
            src2,
            dst: dst.clone(),
        });
        dst
    }

    fn emit_tacky_function(&mut self, f: parse::Function) -> Function {
        let mut body = Vec::new();
        for statement in f.body {
            match statement {
                parse::Statement::Return(expression) => {
                    let value = self.emit_tacky_expr(&expression, &mut body);
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
