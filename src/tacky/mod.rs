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
    parse::{BinaryOperator, BlockItem, Constant, Declaration, ForInit, FunctionDeclaration, StorageClass, Type, UnaryOperator, VariableDeclaration},
    validate::{
        loop_labelling::Statement, type_checker::{self, Expression}, InitialValue, StaticAttr, Symbol, ValidateResult
    },
};
use std::collections::HashMap;

pub struct Tacky {
    counter: u32,
    labels: HashMap<String, u32>,
    validate_result: ValidateResult,
}

impl Tacky {
    pub fn new(validate_result: ValidateResult) -> Self {
        Tacky {
            counter: 0,
            labels: HashMap::new(),
            validate_result,
        }
    }
}

impl Tacky {
    fn make_name(&mut self, name: &str) -> String {
        let name = format!("tacky_{}_{}", name, self.counter);
        self.counter += 1;
        name
    }

    fn make_tacky_var(&mut self, ty: Type) -> Value {
        let name = self.make_name("tmp");
        self.validate_result.symbols.insert(
            name.clone(),
            Symbol::Value(type_checker::symbol::Value::Local(ty.clone())),
        );
        Value::Var(name, ty)
    }

    fn emit_temp(&mut self, src: Value, instructions: &mut Vec<Instruction>) -> Value {
        let dst = self.make_tacky_var(src.parse_type());
        instructions.push(Instruction::Copy {
            src,
            dst: dst.clone(),
        });
        dst
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
        e: &Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        match e {
            Expression::BinOp(op, e1, e2, _) => self.emit_tacky_binop(op, e1, e2, instructions),
            Expression::Assignment(lhs, rhs, _) => match lhs.as_ref() {
                Expression::Var(v, ty) => {
                    let src = self.emit_tacky_expr(rhs, instructions)?;
                    instructions.push(Instruction::Copy {
                        src,
                        dst: Value::Var(v.clone(), ty.clone()),
                    });
                    Ok(Value::Var(v.clone(), ty.clone()))
                }

                e => self.emit_tacky_expr(e, instructions),
            },
            Expression::FunctionCall(ident, params, ty) => {
                let mut args = Vec::new();
                for p in params {
                    let arg = self.emit_tacky_expr(p, instructions)?;
                    args.push(arg);
                }
                let result = self.make_tacky_var(ty.clone());
                instructions.push(Instruction::FunCall {
                    name: ident.clone(),
                    args,
                    dst: result.clone(),
                });
                Ok(result)
            }
            Expression::Cast(ty, expr) => {
                let expr_ty = expr.get_type();
                let expr = self.emit_tacky_expr(expr, instructions)?;
                if expr_ty == *ty {
                    Ok(expr)
                } else {
                    let dst = self.make_tacky_var(ty.clone());
                    match ty {
                        Type::Int => instructions.push(Instruction::Truncate {
                            src: expr,
                            dst: dst.clone(),
                        }),
                        Type::Long => instructions.push(Instruction::SignExtend {
                            src: expr,
                            dst: dst.clone(),
                        }),
                        _ => return Err(CompilerError::InvalidCast(expr_ty, ty.clone())),
                    }
                    Ok(dst)
                }
            }
            e => self.emit_tacky_factor(e, instructions),
        }
    }

    fn emit_tacky_conditional(
        &mut self,
        cond: &Expression,
        then: &Expression,
        els: &Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        let cond = self.emit_tacky_expr(cond, instructions)?;
        let cond = self.emit_temp(cond, instructions);
        let result = self.make_tacky_var(then.get_type());

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
        f: &Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        match f {
            Expression::Constant(i) => Ok(Value::Constant(*i)),
            Expression::Unary(op, inner, _) => self.emit_tacky_unaryop(op, inner, instructions),
            Expression::Var(v, ty) => Ok(Value::Var(v.clone(), ty.clone())),
            Expression::Conditional(cond, then, els, _) => {
                self.emit_tacky_conditional(cond.as_ref(), then.as_ref(), els, instructions)
            }
            e => self.emit_tacky_expr(e, instructions),
        }
    }

    fn emit_tacky_unaryop(
        &mut self,
        op: &UnaryOperator,
        inner: &Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        let dst = self.make_tacky_var(inner.get_type());
        match op {
            UnaryOperator::PreIncrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: src.clone(),
                    src2: Value::Constant(Constant::Int(1)),
                    dst: dst.clone(),
                });
                Ok(dst)
            }
            UnaryOperator::PreDecrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: src.clone(),
                    src2: Value::Constant(Constant::Int(1)),
                    dst: dst.clone(),
                });
                Ok(dst)
            }
            UnaryOperator::PostDecrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Subtract,
                    src1: src.clone(),
                    src2: Value::Constant(Constant::Int(1)),
                    dst: dst.clone(),
                });
                Ok(src)
            }
            UnaryOperator::PostIncrement => {
                let src = self.emit_tacky_expr(inner, instructions)?;
                instructions.push(Instruction::Binary {
                    op: BinaryOp::Add,
                    src1: src.clone(),
                    src2: Value::Constant(Constant::Int(1)),
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

    fn emit_tacky_binop(
        &mut self,
        op: &BinaryOperator,
        e1: &Expression,
        e2: &Expression,
        instructions: &mut Vec<Instruction>,
    ) -> Result<Value, CompilerError> {
        match op {
            BinaryOperator::And => {
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
                let one = Value::Constant(Constant::Int(1));
                let zero = Value::Constant(Constant::Int(0));
                let dst = self.make_tacky_var(Type::Int);
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
            BinaryOperator::Or => {
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
                let one = Value::Constant(Constant::Int(1));
                let zero = Value::Constant(Constant::Int(0));
                let dst = self.make_tacky_var(Type::Int);
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
                //Check this get_type
                let dst = self.make_tacky_var(e1.get_type());
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
        d: &VariableDeclaration<Expression>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let name = d.name.clone();
        let value = d
            .init
            .as_ref()
            .map(|e| self.emit_tacky_expr(e, instructions))
            .transpose()?;

        if d.storage_class == Some(StorageClass::Static) {
            return Ok(());
        }
        if d.storage_class != Some(StorageClass::Extern) {
            let value = value.unwrap_or(Value::Constant(d.var_type.zero_constant()));
            instructions.push(Instruction::Copy {
                src: value,
                dst: Value::Var(name, d.var_type.clone()),
            });
        }
        Ok(())
    }

    fn emit_tacky_decl(
        &mut self,
        d: &Declaration<Statement<Expression>, Expression>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match d {
            Declaration::Variable(decl) => {
                self.emit_tacky_variable_decl(decl, instructions)?;
            }
            Declaration::Function(_) => {}
        }
        Ok(())
    }

    fn emit_tacky_block_item(
        &mut self,
        item: &BlockItem<Statement<Expression>, Expression>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match item {
            BlockItem::Declaration(decl) => {
                self.emit_tacky_decl(decl, instructions)?;
            }
            BlockItem::Statement(s) => {
                self.emit_tacky_statement(s, instructions)?;
            }
        }
        Ok(())
    }

    fn emit_tacky_do_while(
        &mut self,
        body: &Statement<Expression>,
        cond: &Expression,
        loop_label: &str,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
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
        cond: &Expression,
        body: &Statement<Expression>,
        loop_label: &str,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
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
        for_init: &ForInit<Expression>,
        cond: &Option<Expression>,
        post: &Option<Expression>,
        body: &Statement<Expression>,
        loop_label: &str,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        let start_label = self.make_label("start".to_string());
        let break_label = format!("break_{loop_label}");
        let continue_label = format!("continue_{loop_label}");

        match for_init {
            ForInit::InitDeclaration(decl) => {
                self.emit_tacky_variable_decl(decl, instructions)?;
            }
            ForInit::InitExpression(Some(e)) => {
                self.emit_tacky_expr(e, instructions)?; //Value?
            }
            ForInit::InitExpression(None) => {}
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
        s: &Statement<Expression>,
        instructions: &mut Vec<Instruction>,
    ) -> Result<(), CompilerError> {
        match s {
            Statement::If(cond, then, els) => {
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
            Statement::Return(e) => {
                let value = self.emit_tacky_expr(e, instructions)?;
                instructions.push(Instruction::Return(value));
                Ok(())
            }
            Statement::Expression(e) => {
                let _value = self.emit_tacky_expr(e, instructions)?;
                Ok(())
            }
            Statement::Null => Ok(()),
            Statement::Compound(block) => {
                for item in block {
                    self.emit_tacky_block_item(item, instructions)?;
                }
                Ok(())
            }
            Statement::DoWhile(body, cond, loop_label) => {
                self.emit_tacky_do_while(body, cond, loop_label, instructions)
            }
            Statement::While(cond, body, label) => {
                self.emit_tacky_while(cond, body, label, instructions)
            }
            Statement::For(for_init, cond, post, body, loop_label) => {
                self.emit_tacky_for_loop(for_init, cond, post, body, loop_label, instructions)
            }
            Statement::Break(label) => {
                let break_label = format!("break_{}", label.clone());
                instructions.push(Instruction::Jump {
                    target: break_label,
                });
                Ok(())
            }
            Statement::Continue(label) => {
                let continue_label = format!("continue_{}", label.clone());
                instructions.push(Instruction::Jump {
                    target: continue_label,
                });
                Ok(())
            } //s => unimplemented!("Unimplemented Tacky statement {:?}", s),
        }
    }

    fn emit_tacky_function(
        &mut self,
        f: &FunctionDeclaration<Statement<Expression>, Expression>,
    ) -> Result<Option<Function>, CompilerError> {
        let mut body = Vec::new();
        if let Some(body_stmt) = f.body.as_ref() {
            for block_item in body_stmt {
                self.emit_tacky_block_item(block_item, &mut body)?;
            }

            Tacky::fixup_missing_return(&mut body);

            Ok(Some(Function {
                name: f.name.clone(),
                global: self
                    .validate_result
                    .symbols
                    .get(&f.name)
                    .unwrap()
                    .is_global(),
                params: f
                    .parameters
                    .iter()
                    .map(|(ty, name)| (ty.clone(), name.clone()))
                    .collect(),
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
                        init: i.clone(),
                        ty: i.get_type(),
                    }));
                }
                InitialValue::Tentative => {
                    new_symbols.push(TopLevel::StaticVariable(StaticVariable {
                        identifier: name.clone(),
                        global: static_attr.global,
                        init: static_attr.ty.zero_constant().into(),
                        ty: static_attr.ty.clone(),
                    }));
                }
                _ => {}
            }
        }
        new_symbols
    }

    pub fn emit_tacky(&mut self) -> Result<TackyResult, CompilerError> {
        let mut top_level = Vec::new();
        for decl in self
            .validate_result
            .program
            .declarations
            .clone()
            .into_iter()
        {
            match decl {
                Declaration::Function(f) => {
                    let function = self.emit_tacky_function(&f)?;
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
        let static_variables = Self::statics(&self.validate_result.symbols);
        top_level.extend(Tacky::convert_static_variables_to_tacky(&static_variables));
        let result = TackyResult {
            program: Program { top_level },
            symbols: self.validate_result.symbols.clone(),
        };
        Ok(result)
    }

    pub fn fixup_missing_return(instructions: &mut Vec<Instruction>) {
        let last = instructions.last();
        if let Some(Instruction::Return(_)) = last {
            return;
        }
        instructions.push(Instruction::Return(Value::Constant(Constant::Int(0))));
        //Return type doesnt really matter ?
    }

    fn statics(symbol_table: &HashMap<String, Symbol>) -> HashMap<String, StaticAttr> {
        symbol_table
            .iter()
            .filter_map(|(name, symbol)| {
                if let Symbol::Value(type_checker::symbol::Value::Static(static_attr, _)) = &symbol
                {
                    Some((name.clone(), static_attr.clone()))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct TackyResult {
    pub program: Program,
    pub symbols: HashMap<String, Symbol>,
}
