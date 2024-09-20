pub mod binary_op;
pub mod function;
pub mod instruction;
pub mod program;
pub mod unary_op;
pub mod value;

pub use binary_op::*;
pub use function::*;
pub use instruction::*;
pub use program::*;
pub use unary_op::*;
pub use value::*;


use crate::{
    error::CompilerError,
    parse::{self},
};
use std::collections::HashMap;

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
    ) -> Result<Value, CompilerError> {
        match e {
            parse::Expression::BinOp(op, e1, e2) => self.emit_tacky_binop(op, e1, e2, instructions),
            parse::Expression::Assignment(lhs, rhs) => match lhs.as_ref() {
                parse::Expression::Var(v) => {
                    let src = self.emit_tacky_expr(rhs, instructions)?;
                    instructions.push(Instruction::Copy {
                        src,
                        dst: Value::Var(v.clone()),
                    });
                    Ok(Value::Var(v.clone()))
                }
                e => self.emit_tacky_expr(e, instructions),
            },
            e => self.emit_tacky_factor(e, instructions),
        }
    }

    fn emit_tacky_factor(
        &mut self,
        f: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        match f {
            parse::Expression::Constant(i) => Ok(Value::Constant(*i)),
            parse::Expression::Unary(op, inner) => self.emit_tacky_unaryop(op, inner, instructions),
            parse::Expression::Var(v) => Ok(Value::Var(v.clone())),
            e => self.emit_tacky_expr(e, instructions),
        }
    }

    fn emit_tacky_unaryop(
        &mut self,
        op: &parse::UnaryOperator,
        inner: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        let src = self.emit_tacky_factor(inner, instructions)?;
        let dst_name = self.make_name();
        let dst = Value::Var(dst_name);
        let tacky_op = op.into();
        instructions.push(Instruction::Unary {
            op: tacky_op,
            src,
            dst: dst.clone(),
        });
        Ok(dst)
    }

    fn emit_temp(&mut self, src: Value, instructions: &mut Vec<Instruction>) -> Value {
        let name = self.make_name();
        let dst = Value::Var(name.clone());
        instructions.push(Instruction::Copy {
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
    ) -> Result<Value, CompilerError> {
        match op {
            parse::BinaryOperator::And => {
                let v1 = self.emit_tacky_expr(e1, instructions)?;
                let v1 = self.emit_temp(v1, instructions);
                let false_label = self.make_label("else".to_string());
                let end = self.make_label("end".to_string());
                instructions.push(Instruction::JumpIfZero {
                    condition: v1,
                    target: false_label.clone(),
                });
                let v2 = self.emit_tacky_expr(e2, instructions)?;
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
                Ok(dst)
            }
            parse::BinaryOperator::Or => {
                let v1 = self.emit_tacky_expr(e1, instructions)?;
                let v1 = self.emit_temp(v1, instructions);

                let true_label = self.make_label("if".to_string());
                let end = self.make_label("end".to_string());
                instructions.push(Instruction::JumpIfNotZero {
                    condition: v1,
                    target: true_label.clone(),
                });
                let v2 = self.emit_tacky_expr(e2, instructions)?;
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
                Ok(dst)
            }
            _ => {
                let src1 = self.emit_tacky_expr(e1, instructions)?;
                let src2 = self.emit_tacky_expr(e2, instructions)?;
                let dst_name = self.make_name();
                let dst = Value::Var(dst_name);
                let tacky_op = op.try_into()?;
                instructions.push(Instruction::Binary {
                    op: tacky_op,
                    src1,
                    src2,
                    dst: dst.clone(),
                });
                Ok(dst)
            }
        }
    }

    fn emit_tacky_function(&mut self, f: parse::Function) -> Result<Function, CompilerError> {
        let mut body = Vec::new();
        for statement in f.body {
            match statement {
                parse::BlockItem::Statement(parse::Statement::Return(expression)) => {
                    let value = self.emit_tacky_expr(&expression, &mut body)?;
                    body.push(Instruction::Return(value));
                }
                parse::BlockItem::Statement(parse::Statement::Expression(expression)) => {
                    println!("Expr {:?}", expression);
                    let _value = self.emit_tacky_expr(&expression, &mut body)?; //Do I need return value?
                }
                parse::BlockItem::Declaration(decl) => {
                    let name = decl.name;
                    let value = decl
                        .value
                        .map(|e| self.emit_tacky_expr(&e, &mut body))
                        .transpose()?;
                    let value = value.unwrap_or(Value::Constant(0));
                    body.push(Instruction::Copy {
                        src: value,
                        dst: Value::Var(name),
                    });
                }
                parse::BlockItem::Statement(parse::Statement::Null) => (),
                s => unimplemented!("Unimplemented Tacky statement {:?}", s),
            }
        }
        Ok(Function { name: f.name, body })
    }

    pub fn emit_tacky(&mut self, p: parse::Program) -> Result<Program, CompilerError> {
        let function = self.emit_tacky_function(p.function)?;
        Ok(Program { function })
    }
}
