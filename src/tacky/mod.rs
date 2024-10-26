pub mod binary_op;
pub mod instruction;
pub mod program;
pub mod top_level;
pub mod unary_op;
pub mod value;

use anyhow::Result;
pub use binary_op::*;
pub use instruction::*;
pub use program::*;
pub use top_level::*;
pub use unary_op::*;
pub use value::*;

use crate::{
    error::CompilerError,
    parse::{self, Expression},
    validate::{IdentifierAttributes, InitialValue, StaticAttr, Symbol},
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
            parse::Expression::FunctionCall(ident, params) => {
                let mut args = Vec::new();
                for p in params {
                    let arg = self.emit_tacky_expr(p, instructions)?;
                    args.push(arg);
                }
                let result = Value::Var(self.make_name());
                instructions.push(Instruction::FunCall {
                    name: ident.clone(),
                    args,
                    dst: result.clone(),
                });
                Ok(result)
            }
            e => self.emit_tacky_factor(e, instructions),
        }
    }

    fn emit_tacky_conditional(
        &mut self,
        cond: &parse::Expression,
        then: &parse::Expression,
        els: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        let cond = self.emit_tacky_expr(cond, instructions)?;
        let cond = self.emit_temp(cond, instructions);
        let result = Value::Var(self.make_name());

        let else_label = self.make_label("e2_label".to_string());
        let end_label = self.make_label("end".to_string());
        instructions.push(Instruction::JumpIfZero {
            condition: cond,
            target: else_label.clone(),
        });

        let v1 = self.emit_tacky_expr(then, instructions)?;
        instructions.push(Instruction::Copy {
            src: v1,
            dst: result.clone(),
        });

        instructions.push(Instruction::Jump {
            target: end_label.clone(),
        });
        instructions.push(Instruction::Label { name: else_label });

        let v2 = self.emit_tacky_expr(els, instructions)?;
        instructions.push(Instruction::Copy {
            src: v2,
            dst: result.clone(),
        });

        instructions.push(Instruction::Label { name: end_label });
        Ok(result)
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
            parse::Expression::Conditional(cond, then, els) => {
                self.emit_tacky_conditional(cond.as_ref(), then.as_ref(), els, instructions)
            }
            e => self.emit_tacky_expr(e, instructions),
        }
    }

    fn emit_tacky_unaryop(
        &mut self,
        op: &parse::UnaryOperator,
        inner: &parse::Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        let dst_name = self.make_name();
        let dst = Value::Var(dst_name);
        match op {
            parse::UnaryOperator::PreIncrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: src.clone(),
                    src2: Value::Constant(1),
                    dst: dst.clone(),
                });
                Ok(dst)
            }
            parse::UnaryOperator::PreDecrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: src.clone(),
                    src2: Value::Constant(1),
                    dst: dst.clone(),
                });
                Ok(dst)
            }
            parse::UnaryOperator::PostDecrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: src.clone(),
                    src2: Value::Constant(1),
                    dst: dst.clone(),
                });
                Ok(src)
            }
            parse::UnaryOperator::PostIncrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: src.clone(),
                    src2: Value::Constant(1),
                    dst: dst.clone(),
                });
                Ok(src)
            }
            _ => {
                let src = self.emit_tacky_factor(inner, instructions)?;
                let tacky_op = op.into();
                instructions.push(Instruction::Unary {
                    op: tacky_op,
                    src,
                    dst: dst.clone(),
                });
                Ok(dst)
            }
        }
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

    fn emit_tacky_variable_decl(
        &mut self,
        d: &parse::VariableDeclaration,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let name = d.name.clone();
        let value = d
            .init
            .as_ref()
            .map(|e| self.emit_tacky_expr(e, instructions))
            .transpose()?;

        //TODO Check if this is right? We have already moved to symbols. Fuck it.
        if d.storage_class == Some(parse::StorageClass::Static) {
            return Ok(());
        }
        if d.storage_class != Some(parse::StorageClass::Extern) {
            let value = value.unwrap_or(Value::Constant(0));
            instructions.push(Instruction::Copy {
                src: value,
                dst: Value::Var(name),
            });
        }
        Ok(())
    }

    fn emit_tacky_decl(
        &mut self,
        d: &parse::Declaration,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match d {
            parse::Declaration::Variable(decl) => {
                self.emit_tacky_variable_decl(decl, instructions)?;
            }
            parse::Declaration::Function(_) => {}
        }
        Ok(())
    }

    fn emit_tacky_block_item(
        &mut self,
        item: &parse::BlockItem,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match item {
            parse::BlockItem::Declaration(decl) => {
                self.emit_tacky_decl(decl, instructions)?;
            }
            parse::BlockItem::Statement(s) => {
                self.emit_tacky_statement(s, instructions)?;
            }
        }
        Ok(())
    }

    fn emit_tacky_do_while(
        &mut self,
        body: &parse::Statement,
        cond: &parse::Expression,
        loop_label: &Option<String>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let loop_label = loop_label.clone().unwrap();
        let start_label = self.make_label("start".to_string());
        let break_label = format!("break_{loop_label}");
        let continue_label = format!("continue_{loop_label}");

        instructions.push(Instruction::Label {
            name: start_label.clone(),
        });
        self.emit_tacky_statement(body, instructions)?;
        instructions.push(Instruction::Label {
            name: continue_label.clone(),
        });

        let cond = self.emit_tacky_expr(cond, instructions)?;

        instructions.push(Instruction::JumpIfNotZero {
            condition: cond,
            target: start_label.clone(),
        });
        instructions.push(Instruction::Label {
            name: break_label.clone(),
        });
        Ok(())
    }

    fn emit_tacky_while(
        &mut self,
        cond: &parse::Expression,
        body: &parse::Statement,
        loop_label: &Option<String>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let loop_label = loop_label.clone().unwrap();
        let break_label = format!("break_{loop_label}");
        let continue_label = format!("continue_{loop_label}");
        instructions.push(Instruction::Label {
            name: continue_label.clone(),
        });

        let cond = self.emit_tacky_expr(cond, instructions)?;
        instructions.push(Instruction::JumpIfZero {
            condition: cond,
            target: break_label.clone(),
        });
        self.emit_tacky_statement(body, instructions)?;
        instructions.push(Instruction::Jump {
            target: continue_label.clone(),
        });
        instructions.push(Instruction::Label {
            name: break_label.clone(),
        });
        Ok(())
    }

    fn emit_tacky_for_loop(
        &mut self,
        for_init: &parse::ForInit,
        cond: &Option<Expression>,
        post: &Option<Expression>,
        body: &parse::Statement,
        loop_label: &Option<String>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let loop_label = loop_label.clone().unwrap();
        let start_label = self.make_label("start".to_string());
        let break_label = format!("break_{loop_label}");
        let continue_label = format!("continue_{loop_label}");

        match for_init {
            parse::ForInit::InitDeclaration(decl) => {
                self.emit_tacky_variable_decl(decl, instructions)?;
            }
            parse::ForInit::InitExpression(Some(e)) => {
                self.emit_tacky_expr(e, instructions)?; //Value?
            }
            parse::ForInit::InitExpression(None) => {}
        }

        instructions.push(Instruction::Label {
            name: start_label.clone(),
        });

        if let Some(cond) = cond {
            let cond = self.emit_tacky_expr(cond, instructions)?;
            instructions.push(Instruction::JumpIfZero {
                condition: cond,
                target: break_label.clone(),
            });
        }

        self.emit_tacky_statement(body, instructions)?;
        instructions.push(Instruction::Label {
            name: continue_label.clone(),
        });

        if let Some(post) = post {
            self.emit_tacky_expr(post, instructions)?;
        }
        instructions.push(Instruction::Jump {
            target: start_label.clone(),
        });

        instructions.push(Instruction::Label {
            name: break_label.clone(),
        });

        Ok(())
    }

    fn emit_tacky_statement(
        &mut self,
        s: &parse::Statement,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match s {
            parse::Statement::If(cond, then, els) => {
                let cond = self.emit_tacky_expr(cond, instructions)?;
                let cond = self.emit_temp(cond, instructions);
                let else_label = self.make_label("else".to_string());
                let end_label = self.make_label("end".to_string());

                match els {
                    None => {
                        instructions.push(Instruction::JumpIfZero {
                            condition: cond,
                            target: end_label.clone(),
                        });
                        self.emit_tacky_statement(then, instructions)?;
                        instructions.push(Instruction::Label { name: end_label });
                    }
                    Some(els) => {
                        instructions.push(Instruction::JumpIfZero {
                            condition: cond,
                            target: else_label.clone(),
                        });
                        self.emit_tacky_statement(then, instructions)?;
                        instructions.push(Instruction::Jump {
                            target: end_label.clone(),
                        });
                        instructions.push(Instruction::Label { name: else_label });
                        self.emit_tacky_statement(els, instructions)?;
                        instructions.push(Instruction::Label { name: end_label });
                    }
                }

                Ok(())
            }
            parse::Statement::Return(e) => {
                let value = self.emit_tacky_expr(e, instructions)?;
                instructions.push(Instruction::Return(value));
                Ok(())
            }
            parse::Statement::Expression(e) => {
                let _value = self.emit_tacky_expr(e, instructions)?;
                Ok(())
            }
            parse::Statement::Null => Ok(()),
            parse::Statement::Compound(block) => {
                for item in block {
                    self.emit_tacky_block_item(item, instructions)?;
                }
                Ok(())
            }
            parse::Statement::DoWhile(body, cond, loop_label) => {
                self.emit_tacky_do_while(body, cond, loop_label, instructions)
            }
            parse::Statement::While(cond, body, label) => {
                self.emit_tacky_while(cond, body, label, instructions)
            }
            parse::Statement::For(for_init, cond, post, body, loop_label) => {
                self.emit_tacky_for_loop(for_init, cond, post, body, loop_label, instructions)
            }
            parse::Statement::Break(label) => {
                let break_label = format!("break_{}", label.clone().unwrap());
                instructions.push(Instruction::Jump {
                    target: break_label,
                });
                Ok(())
            }
            parse::Statement::Continue(label) => {
                let continue_label = format!("continue_{}", label.clone().unwrap());
                instructions.push(Instruction::Jump {
                    target: continue_label,
                });
                Ok(())
            } //s => unimplemented!("Unimplemented Tacky statement {:?}", s),
        }
    }

    fn emit_tacky_function(
        &mut self,
        f: parse::FunctionDeclaration,
        symbol_table: &HashMap<String, Symbol>,
    ) -> Result<Option<Function>, CompilerError> {
        let mut body = Vec::new();
        if let Some(body_stmt) = f.body {
            for block_item in body_stmt {
                self.emit_tacky_block_item(&block_item, &mut body)?;
            }

            Tacky::fixup_missing_return(&mut body);

            Ok(Some(Function {
                name: f.name.clone(),
                global: symbol_table.get(&f.name).unwrap().attributes.is_global(),
                params: f.parameters,
                body: Some(body),
            }))
        } else {
            Ok(None)
        }
    }

    fn convert_static_variables_to_tacky(
        static_variables: &HashMap<String, StaticAttr>,
    ) -> Vec<TopLevel> {
        let mut new_symbols = Vec::new();
        for (name, static_attr) in static_variables {
            match &static_attr.init {
                InitialValue::Initial(i) => {
                    new_symbols.push(TopLevel::StaticVariable(StaticVariable {
                        identifier: name.clone(),
                        global: static_attr.global,
                        init: *i,
                    }));
                }
                InitialValue::Tentative => {
                    new_symbols.push(TopLevel::StaticVariable(StaticVariable {
                        identifier: name.clone(),
                        global: static_attr.global,
                        init: 0,
                    }));
                }
                _ => {}
            }
        }
        new_symbols
    }

    pub fn emit_tacky(
        &mut self,
        p: parse::Program,
        symbol_table: &HashMap<String, Symbol>,
    ) -> Result<(Program, HashMap<String, StaticAttr>), CompilerError> {
        let mut top_level = Vec::new();
        for decl in p.declarations {
            match decl {
                parse::Declaration::Function(f) => {
                    let function = self.emit_tacky_function(f, symbol_table)?;
                    if let Some(function) = function {
                        top_level.push(TopLevel::Function(function));
                    }
                }
                _ => {
                    //Handled by convert_symbols_to_tacky
                }
            }
        }

        //Walk the tree and emit the static variables

        let static_variables = Self::statics(symbol_table);
        top_level.extend(Tacky::convert_static_variables_to_tacky(&static_variables));
        Ok((Program { top_level }, static_variables))
    }

    /*fn extract_symbols(p: &parse::Program, symbols : &mut Vec<Symbol>) {
        let mut symbols = HashMap::new();
        for decl in &p.declarations {
            match decl {
                parse::Declaration::Function(f) => {
                    for body in f.body.unwrap() {
                        match body {
                            parse::BlockItem::Declaration(d) => {
                                match d {
                                    parse::VariableDeclaration { name, value, .. } => {
                                        symbols.push(name.clone(), Symbol::new(name.clone()));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    symbols.insert(v.name.clone(), Symbol::new(v.clone()));
                }
                _ => {}
            }
        }
        symbols
    }*/

    pub fn fixup_missing_return(instructions: &mut Vec<Instruction>) {
        let last = instructions.last();
        if let Some(Instruction::Return(_)) = last {
            return;
        }
        instructions.push(Instruction::Return(Value::Constant(0)));
    }

    fn statics(symbol_table: &HashMap<String, Symbol>) -> HashMap<String, StaticAttr> {
        symbol_table
            .iter()
            .filter_map(|(name, symbol)| {
                if let IdentifierAttributes::Static(static_attr) = &symbol.attributes {
                    Some((name.clone(), static_attr.clone()))
                } else {
                    None
                }
            })
            .collect()
    }
}
