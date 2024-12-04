pub mod ast;
pub mod symbol;

use super::error::SemanticAnalysisError;
use super::loop_labelling::Statement;
use crate::ast::*;
use crate::{
    ast::{
        BinaryOperator, Declaration, ForInit, FunctionDeclaration, StorageClass, Type,
        UnaryOperator, VariableDeclaration,
    },
    error::*,
};
pub use ast::Expression;
use std::collections::HashMap;
pub use symbol::*;

#[derive(Default)]
pub(crate) struct TypeChecker {
    pub symbol_table: HashMap<String, Symbol>,
}

impl TypeChecker {
    //This is fugly. Can we do better?
    fn compile_cast(i: InitialValue, ty: &Type) -> InitialValue {
        match i {
            InitialValue::Initial(init) => {
                let new_init = match (init, ty) {
                    (StaticInit::IntInit(val), Type::Int) => StaticInit::IntInit(val),
                    (StaticInit::IntInit(val), Type::Long) => StaticInit::LongInit(val as i64),
                    (StaticInit::IntInit(val), Type::UInt) => StaticInit::UIntInit(val as u32),
                    (StaticInit::IntInit(val), Type::ULong) => StaticInit::ULongInit(val as u64),
                    (StaticInit::LongInit(val), Type::Int) => StaticInit::IntInit(val as i32),
                    (StaticInit::LongInit(val), Type::Long) => StaticInit::LongInit(val),
                    (StaticInit::LongInit(val), Type::UInt) => StaticInit::UIntInit(val as u32),
                    (StaticInit::LongInit(val), Type::ULong) => StaticInit::ULongInit(val as u64),
                    (StaticInit::UIntInit(val), Type::Int) => StaticInit::IntInit(val as i32),
                    (StaticInit::UIntInit(val), Type::Long) => StaticInit::LongInit(val as i64),
                    (StaticInit::UIntInit(val), Type::UInt) => StaticInit::UIntInit(val),
                    (StaticInit::UIntInit(val), Type::ULong) => StaticInit::ULongInit(val as u64),
                    (StaticInit::ULongInit(val), Type::Int) => StaticInit::IntInit(val as i32),
                    (StaticInit::ULongInit(val), Type::Long) => StaticInit::LongInit(val as i64),
                    (StaticInit::ULongInit(val), Type::UInt) => StaticInit::UIntInit(val as u32),
                    (StaticInit::ULongInit(val), Type::ULong) => StaticInit::ULongInit(val),
                    _ => panic!("Invalid cast"),
                };
                InitialValue::Initial(new_init)
            }
            _ => i,
        }
    }

    pub fn type_check_file_scope_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<crate::parse::Expression>,
    ) -> Result<VariableDeclaration<Expression>, CompilerError> {
        let mut initial_value =
            if let Some(crate::parse::Expression::Constant(i)) = &variable_declaration.init {
                InitialValue::Initial((*i).into())
            } else if variable_declaration.init.is_none() {
                if variable_declaration.storage_class == Some(StorageClass::Extern) {
                    InitialValue::NoInitializer
                } else {
                    InitialValue::Tentative
                }
            } else {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidInitializerForFileScopeVariable,
                ));
            };

        let mut global = variable_declaration.storage_class != Some(StorageClass::Static);

        if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
            if old_decl.is_function() {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::FunctionRedclaredAsVariable(
                        variable_declaration.name.clone(),
                    ),
                ));
            }
            if old_decl.get_type() != variable_declaration.var_type {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ConflictingTypeDefinition(
                        variable_declaration.name.clone(),
                    ),
                ));
            }
            if variable_declaration.storage_class == Some(StorageClass::Extern) {
                global = old_decl.is_global();
            } else if old_decl.is_global() != global {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ConflictingVariableLinkage(
                        variable_declaration.name.clone(),
                    ),
                ));
            }

            if let InitialValue::Initial(_) = old_decl.init() {
                if let InitialValue::Initial(_) = initial_value {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::ConflictingFileScopeVariableDefinitions(
                            variable_declaration.name.clone(),
                        ),
                    ));
                } else {
                    initial_value = old_decl.init();
                }
            } else if !initial_value.is_constant() && old_decl.init().is_tentative() {
                initial_value = InitialValue::Tentative;
            }
        }

        let init = Self::compile_cast(initial_value, &variable_declaration.var_type);
        let ty = init.get_type(variable_declaration.var_type.clone());
        self.symbol_table.insert(
            variable_declaration.name.clone(),
            Symbol::Value(Value::Static(
                StaticAttr {
                    init: init.clone(),
                    global,
                    ty: ty.clone(),
                },
                ty.clone(),
            )),
        );

        let expr = match init {
            InitialValue::Initial(i) => Some(i.into()),
            _ => None,
        };

        let variable_declaration = VariableDeclaration {
            name: variable_declaration.name.clone(),
            var_type: ty.clone(),
            init: expr,
            storage_class: variable_declaration.storage_class.clone(),
        };

        Ok(variable_declaration.clone())
    }

    fn type_check_local_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<crate::parse::Expression>,
    ) -> Result<VariableDeclaration<Expression>, CompilerError> {
        if variable_declaration.storage_class == Some(StorageClass::Extern) {
            if variable_declaration.init.is_some() {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ExternVariableCannotHaveInitializer, //Wrong wrror
                ));
            }
            if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
                if old_decl.is_function() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionRedclaredAsVariable(
                            variable_declaration.name.clone(),
                        ),
                    ));
                }
                if old_decl.get_type() != variable_declaration.var_type {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::ConflictingVariableLinkage(
                            //Wrong error
                            variable_declaration.name.clone(),
                        ),
                    ));
                }
            } else {
                self.symbol_table.insert(
                    variable_declaration.name.clone(),
                    Symbol::Value(Value::Static(
                        StaticAttr {
                            init: InitialValue::NoInitializer,
                            global: true,
                            ty: variable_declaration.var_type.clone(),
                        },
                        variable_declaration.var_type.clone(),
                    )),
                );
            }
        } else if variable_declaration.storage_class == Some(StorageClass::Static) {
            let initial_value =
                if let Some(crate::parse::Expression::Constant(i)) = &variable_declaration.init {
                    InitialValue::Initial((*i).into())
                } else if variable_declaration.init.is_none() {
                    InitialValue::Tentative
                } else {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::NonConstantInitializerForLocalStaticVariable,
                    ));
                };

            let init = Self::compile_cast(initial_value, &variable_declaration.var_type);
            let ty = init.get_type(variable_declaration.var_type.clone());

            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::Value(Value::Static(
                    StaticAttr {
                        init,
                        global: false,
                        ty: ty.clone(),
                    },
                    ty.clone(),
                )),
            );
        } else {
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::Value(Value::Local(variable_declaration.var_type.clone())),
            );

            let expr = if let Some(initializer) = variable_declaration.init.as_ref() {
                //Not sure if this is correct
                let initializer = self.type_check_expression(initializer)?;
                let initializer =
                    self.convert_to(variable_declaration.var_type.clone(), &initializer);
                Some(initializer)
            } else {
                None
            };
            let variable_declaration = VariableDeclaration {
                name: variable_declaration.name.clone(),
                var_type: variable_declaration.var_type.clone(),
                init: expr,
                storage_class: variable_declaration.storage_class.clone(),
            };
            return Ok(variable_declaration);
        }

        let variable_declaration = VariableDeclaration {
            name: variable_declaration.name.clone(),
            var_type: variable_declaration.var_type.clone(),
            init: None,
            storage_class: variable_declaration.storage_class.clone(),
        };
        Ok(variable_declaration)
    }

    fn get_common_type(ty1: Type, ty2: Type) -> Type {
        if ty1 == ty2 {
            ty1
        } else if ty1.size() == ty2.size() {
            if ty1.is_signed() {
                return ty2;
            } else {
                return ty1;
            }
        } else if ty1.size().unwrap_or(0) > ty2.size().unwrap_or(0) {
            return ty1;
        } else {
            return ty2;
        }
    }

    fn convert_to(&self, ty: Type, expression: &Expression) -> Expression {
        let expression_ty = expression.get_type();
        if expression_ty == ty.clone() {
            return expression.clone();
        }

        Expression::Cast(ty.clone(), Box::new(expression.clone()))
    }

    fn type_check_expression(
        &self,
        expression: &crate::parse::Expression,
    ) -> Result<Expression, CompilerError> {
        match expression {
            crate::parse::Expression::FunctionCall(name, arguments) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    if let Symbol::FunType(_, expected_args, ty) = symbol {
                        if expected_args.len() != arguments.len() {
                            return Err(CompilerError::SemanticAnalysis(
                                SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                            ));
                        }
                        let converted_arguments = arguments
                            .iter()
                            .zip(expected_args.iter())
                            .map(|(arg, ty)| {
                                let arg = self.type_check_expression(arg)?;
                                let ty = ty.clone();
                                let converted = self.convert_to(ty, &arg);
                                Ok(converted)
                            })
                            .collect::<Result<Vec<Expression>, CompilerError>>()?;

                        let function_call = Expression::FunctionCall(
                            name.clone(),
                            converted_arguments.clone(),
                            ty.clone(),
                        );
                        Ok(function_call)
                    } else {
                        Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ))
                    }
                } else {
                    Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                    ))
                }
            }
            crate::parse::Expression::Var(name) => {
                if let Some(existing) = self.symbol_table.get(name) {
                    if existing.is_function() {
                        return Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ));
                    } else {
                        let ty = existing.get_type();
                        return Ok(Expression::Var(name.clone(), ty));
                    }
                }
                Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::VariableNotDeclared(name.clone()),
                ))
            }
            crate::parse::Expression::BinOp(op, left, right) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                match op {
                    BinaryOperator::And | BinaryOperator::Or => Ok(Expression::BinOp(
                        op.clone(),
                        Box::new(left),
                        Box::new(right),
                        Type::Int,
                    )),
                    _ => {
                        let common_type = Self::get_common_type(left.get_type(), right.get_type());
                        let left = self.convert_to(common_type.clone(), &left);
                        let right = self.convert_to(common_type.clone(), &right);
                        match op {
                            BinaryOperator::Add
                            | BinaryOperator::Sub
                            | BinaryOperator::Mul
                            | BinaryOperator::Div
                            | BinaryOperator::Mod => Ok(Expression::BinOp(
                                op.clone(),
                                Box::new(left),
                                Box::new(right),
                                common_type,
                            )),
                            _ => Ok(Expression::BinOp(
                                op.clone(),
                                Box::new(left),
                                Box::new(right),
                                Type::Int,
                            )),
                        }
                    }
                }
            }
            crate::parse::Expression::Unary(UnaryOperator::Not, expression) => {
                let expression = self.type_check_expression(expression)?;
                let ty = expression.get_type();
                Ok(Expression::Unary(
                    UnaryOperator::Not,
                    Box::new(expression),
                    ty,
                ))
            }
            crate::parse::Expression::Unary(op, expression) => {
                let expression = self.type_check_expression(expression)?;
                let ty = expression.get_type();
                Ok(Expression::Unary(op.clone(), Box::new(expression), ty))
            }
            crate::parse::Expression::Assignment(left, right) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                let ty = left.get_type();
                let converted_right = self.convert_to(ty.clone(), &right);

                Ok(Expression::Assignment(
                    Box::new(left),
                    Box::new(converted_right),
                    ty,
                ))
            }
            crate::parse::Expression::Conditional(condition, then_expression, else_expression) => {
                let condition = self.type_check_expression(condition)?;
                let then_expression = self.type_check_expression(then_expression)?;
                let else_expression = self.type_check_expression(else_expression)?;
                let ty =
                    Self::get_common_type(then_expression.get_type(), else_expression.get_type());

                Ok(Expression::Conditional(
                    Box::new(condition),
                    Box::new(self.convert_to(ty.clone(), &then_expression)),
                    Box::new(self.convert_to(ty.clone(), &else_expression)),
                    ty,
                ))
            }
            crate::parse::Expression::Constant(c) => Ok(Expression::Constant(*c)),
            crate::parse::Expression::Cast(ty, expr) => {
                if let crate::parse::Expression::Assignment(_, _) = expr.as_ref() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidCastInAssignment,
                    ));
                }
                Ok(self.convert_to(ty.clone(), &self.type_check_expression(expr)?))
            }
        }
    }

    fn type_check_for_init(
        &mut self,
        for_init: &ForInit<crate::parse::Expression>,
    ) -> Result<ForInit<Expression>, CompilerError> {
        match for_init {
            ForInit::InitExpression(Some(expression)) => Ok(ForInit::InitExpression(Some(
                self.type_check_expression(expression)?,
            ))),
            ForInit::InitExpression(None) => Ok(ForInit::InitExpression(None)),
            ForInit::InitDeclaration(declaration) => {
                if declaration.storage_class.is_some() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::StaticValueNotValidInForLoopInitializer,
                    ));
                }
                let declaration = self.type_check_local_variable_declaration(declaration)?;
                Ok(ForInit::InitDeclaration(declaration))
            }
        }
    }

    fn type_check_statement(
        &mut self,
        statement: &Statement<crate::parse::Expression>,
        containing_function_name: &str,
    ) -> Result<Statement<Expression>, CompilerError> {
        match statement {
            Statement::Expression(expression) => Ok(Statement::Expression(
                self.type_check_expression(expression)?,
            )),
            Statement::Return(expression) => {
                let return_type = self
                    .symbol_table
                    .get(containing_function_name)
                    .unwrap()
                    .get_type();

                let expression = self.type_check_expression(expression)?;
                Ok(Statement::Return(self.convert_to(return_type, &expression)))
            }
            Statement::If(condition, then_block, else_block) => {
                let condition = self.type_check_expression(condition)?;
                let then_block =
                    self.type_check_statement(then_block.as_ref(), containing_function_name)?;
                let else_block = if let Some(statement) = else_block {
                    Some(Box::new(
                        self.type_check_statement(statement, containing_function_name)?,
                    ))
                } else {
                    None
                };
                Ok(Statement::If(condition, Box::new(then_block), else_block))
            }
            Statement::While(condition, block, loop_label) => {
                let condition = self.type_check_expression(condition)?;
                let block = self.type_check_statement(block, containing_function_name)?;
                Ok(Statement::While(
                    condition,
                    Box::new(block),
                    loop_label.clone(),
                ))
            }
            Statement::DoWhile(body, condition, loop_label) => {
                let body = self.type_check_statement(body, containing_function_name)?;
                let condition = self.type_check_expression(condition)?;
                Ok(Statement::DoWhile(
                    Box::new(body),
                    condition,
                    loop_label.clone(),
                ))
            }
            Statement::For(for_init, condition, post, block, loop_label) => {
                let for_init = self.type_check_for_init(for_init)?;
                let condition = if let Some(condition) = condition {
                    Some(self.type_check_expression(condition)?)
                } else {
                    None
                };
                let post = if let Some(post) = post {
                    Some(self.type_check_expression(post)?)
                } else {
                    None
                };
                let block = self.type_check_statement(block, containing_function_name)?;
                Ok(Statement::For(
                    for_init,
                    condition,
                    post,
                    Box::new(block),
                    loop_label.clone(),
                ))
            }
            Statement::Compound(block_items) => {
                let mut new_block_items = Vec::new();
                for block_item in block_items {
                    new_block_items
                        .push(self.type_check_block_item(block_item, containing_function_name)?);
                }
                Ok(Statement::Compound(new_block_items))
            }
            Statement::Null => Ok(Statement::Null),
            Statement::Continue(loop_label) => Ok(Statement::Continue(loop_label.clone())),
            Statement::Break(loop_label) => Ok(Statement::Break(loop_label.clone())),
        }
    }

    fn type_check_block_item(
        &mut self,
        block_item: &BlockItem<Statement<crate::parse::Expression>, crate::parse::Expression>,
        containing_function_name: &str,
    ) -> Result<BlockItem<Statement<Expression>, Expression>, CompilerError> {
        match block_item {
            BlockItem::Statement(statement) => Ok(BlockItem::Statement(
                self.type_check_statement(statement, containing_function_name)?,
            )),
            BlockItem::Declaration(declaration) => match declaration {
                Declaration::Variable(variable_declaration) => {
                    Ok(BlockItem::Declaration(Declaration::Variable(
                        self.type_check_local_variable_declaration(variable_declaration)?,
                    )))
                }
                Declaration::Function(function_declaration) => {
                    Ok(BlockItem::Declaration(Declaration::Function(
                        self.type_check_function_declaration(function_declaration, false)?,
                    )))
                }
            },
        }
    }

    pub fn type_check_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration<
            Statement<crate::parse::Expression>,
            crate::parse::Expression,
        >,
        top_level: bool,
    ) -> Result<FunctionDeclaration<Statement<Expression>, Expression>, CompilerError> {
        let new_parameters: Vec<_> = function_declaration
            .parameters
            .iter()
            .map(|(ty, _)| ty.clone())
            .collect();
        let function_return_type = function_declaration.fun_type.clone();
        let has_body = function_declaration.body.is_some();
        let mut already_defined = false;
        let mut global = function_declaration.storage_class != Some(StorageClass::Static);
        if let Some(symbol) = self.symbol_table.get(&function_declaration.name) {
            if let Symbol::FunType(old_fun_attr, old_parameters, old_return_type) = symbol {
                //check parameters better
                if old_parameters != &new_parameters || old_return_type != &function_return_type {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::IncompatibleFunctionDeclarations,
                    ));
                }
                already_defined = old_fun_attr.defined;
                if already_defined && has_body {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionAlreadyDeclared(
                            function_declaration.name.clone(),
                        ),
                    ));
                }

                if old_fun_attr.global
                    && function_declaration.storage_class == Some(StorageClass::Static)
                {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::StaticFunctionDeclarationFollowsNonStatic(
                            function_declaration.name.clone(),
                        ),
                    ));
                }
                global = old_fun_attr.global;
            } else {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::FunctionRedclaredAsVariable(
                        function_declaration.name.clone(),
                    ),
                ));
            }
        }

        if !top_level && function_declaration.storage_class == Some(StorageClass::Static) {
            return Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::NonStaticFunctionDeclarationInBlock(
                    function_declaration.name.clone(),
                ),
            ));
        }

        let new_attrs = FunAttr {
            defined: has_body || already_defined,
            global,
        };
        self.symbol_table
            .insert(function_declaration.name.clone(), {
                Symbol::FunType(new_attrs, new_parameters, function_return_type)
            });

        let new_body = if let Some(body) = function_declaration.body.as_ref() {
            for (ty, param) in &function_declaration.parameters {
                self.symbol_table
                    .insert(param.clone(), Symbol::Value(Value::Local(ty.clone())));
            }
            let mut new_body = Vec::new();
            for block_item in body {
                new_body.push(self.type_check_block_item(block_item, &function_declaration.name)?);
            }
            Some(new_body)
        } else {
            None
        };

        let function_declaration = FunctionDeclaration::new(
            function_declaration.name.clone(),
            function_declaration.parameters.clone(),
            new_body,
            function_declaration.fun_type.clone(),
            function_declaration.storage_class.clone(),
        );

        Ok(function_declaration)
    }
}
