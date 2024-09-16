use std::collections::HashMap;

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
    Copy {
        src: Value,
        dst: Value,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: Value,
        target: String,
    },
    JumpIfNotZero {
        condition: Value,
        target: String,
    },
    Label {
        name: String,
    },
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(i32),
    Var(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

fn convert_unop(op: &parse::UnaryOperator) -> UnaryOp {
    match op {
        parse::UnaryOperator::Negation => UnaryOp::Negate,
        parse::UnaryOperator::Tilde => UnaryOp::Complement,
        parse::UnaryOperator::Not => UnaryOp::Not,
        //_ => unimplemented!(),
    }
}

fn convert_binop(op: &parse::BinaryOperator) -> BinaryOp {
    match op {
        parse::BinaryOperator::Add => BinaryOp::Add,
        parse::BinaryOperator::Sub => BinaryOp::Subtract,
        parse::BinaryOperator::Mul => BinaryOp::Multiply,
        parse::BinaryOperator::Div => BinaryOp::Divide,
        parse::BinaryOperator::Mod => BinaryOp::Remainder,
        parse::BinaryOperator::ShiftLeft => BinaryOp::ShiftLeft,
        parse::BinaryOperator::ShiftRight => BinaryOp::ShiftRight,
        parse::BinaryOperator::BitwiseAnd => BinaryOp::BitwiseAnd,
        parse::BinaryOperator::BitwiseOr => BinaryOp::BitwiseOr,
        parse::BinaryOperator::BitwiseXor => BinaryOp::BitwiseXor,
        parse::BinaryOperator::Equal => BinaryOp::Equal,
        parse::BinaryOperator::NotEqual => BinaryOp::NotEqual,
        parse::BinaryOperator::LessThan => BinaryOp::LessThan,
        parse::BinaryOperator::LessThanOrEqual => BinaryOp::LessThanOrEqual,
        parse::BinaryOperator::GreaterThan => BinaryOp::GreaterThan,
        parse::BinaryOperator::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
        //And?
        _ => unimplemented!(),
    }
}

#[derive(Default)]
pub struct Tacky {
    counter: u32,
    labels: HashMap<String, u32>,
}

impl Tacky {
    fn make_name(&mut self) -> String {
        let name = format!("tacky{}", self.counter);
        self.counter += 1;
        name
    }

    fn make_label(&mut self, label: String) -> String {
        let name = format!(
            "{}{}",
            label.clone(),
            self.labels.entry(label.clone()).or_insert(0)
        );
        self.labels.insert(label.clone(), self.labels[&label] + 1);
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

    fn emit_temp(&mut self, src: Value, instructions: &mut Vec<Instruction>) -> Value {
        let name = self.make_name();
        let dst = Value::Var(name.clone());
        instructions.push(Instruction::Copy {
            src: src,
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
        match op {
            parse::BinaryOperator::And => {
                let v1 = self.emit_tacky_expr(e1, instructions);
                let v1 = self.emit_temp(v1, instructions);
                let false_label = self.make_label("else".to_string());
                let end = self.make_label("end".to_string());
                instructions.push(Instruction::JumpIfZero {
                    condition: v1,
                    target: false_label.clone(),
                });
                let v2 = self.emit_tacky_expr(e2, instructions);
                let v2 = self.emit_temp(v2, instructions);
                instructions.push(Instruction::JumpIfZero {
                    condition: v2,
                    target: false_label.clone(),
                });
                let one = Value::Constant(1);
                let zero = Value::Constant(0);
                let dst = Value::Var(self.make_name());
                instructions.push(Instruction::Copy {
                    src: one,
                    dst: dst.clone(),
                });
                instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                instructions.push(Instruction::Label { name: false_label });
                instructions.push(Instruction::Copy {
                    src: zero,
                    dst: dst.clone(),
                });
                instructions.push(Instruction::Label { name: end });
                dst
            }
            parse::BinaryOperator::Or => {
                let v1 = self.emit_tacky_expr(e1, instructions);
                let v1 = self.emit_temp(v1, instructions);

                let true_label = self.make_label("if".to_string());
                let end = self.make_label("end".to_string());
                instructions.push(Instruction::JumpIfNotZero {
                    condition: v1,
                    target: true_label.clone(),
                });
                let v2 = self.emit_tacky_expr(e2, instructions);
                let v2 = self.emit_temp(v2, instructions);

                instructions.push(Instruction::JumpIfNotZero {
                    condition: v2,
                    target: true_label.clone(),
                });
                let one = Value::Constant(1);
                let zero = Value::Constant(0);
                let dst = Value::Var(self.make_name());
                instructions.push(Instruction::Copy {
                    src: zero,
                    dst: dst.clone(),
                });
                instructions.push(Instruction::Jump {
                    target: end.clone(),
                });
                instructions.push(Instruction::Label { name: true_label });
                instructions.push(Instruction::Copy {
                    src: one,
                    dst: dst.clone(),
                });
                instructions.push(Instruction::Label { name: end });
                dst
            }
            _ => {
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
        }
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
