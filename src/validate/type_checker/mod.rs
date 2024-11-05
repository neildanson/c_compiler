pub mod ast;
pub mod symbol;

use super::loop_labelling::LLStatement;
use crate::{error::*, parse::*};
pub use ast::TCExpression;
use std::collections::HashMap;
pub use symbol::*;

#[derive(Default)]
pub(crate) struct TypeChecker {
    pub symbol_table: HashMap<String, Symbol>,
}

impl TypeChecker {
    pub fn type_check_file_scope_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<Expression>,
    ) -> Result<VariableDeclaration<TCExpression>, CompilerError> {
        let mut initial_value = if let Some(Expression::Constant(i)) = &variable_declaration.init {
            InitialValue::Initial(i.clone().into())
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
                    //TODO
                    SemanticAnalysisError::ConflictingVariableLinkage(
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

        self.symbol_table.insert(
            variable_declaration.name.clone(),
            Symbol::Value(Value::Static(
                StaticAttr {
                    init: initial_value,
                    global,
                },
                variable_declaration.var_type.clone(),
            )),
        );

        let variable_declaration = VariableDeclaration {
            name: variable_declaration.name.clone(),
            var_type: variable_declaration.var_type.clone(),
            init: variable_declaration
                .init
                .as_ref()
                .map(|expr| TCExpression::with_type(expr, variable_declaration.var_type.clone())),
            storage_class: variable_declaration.storage_class.clone(),
        };

        Ok(variable_declaration.clone())
    }

    fn type_check_local_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<Expression>,
    ) -> Result<VariableDeclaration<TCExpression>, CompilerError> {
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
                        },
                        variable_declaration.var_type.clone(),
                    )),
                );
            }
        } else if variable_declaration.storage_class == Some(StorageClass::Static) {
            let initial_value = if let Some(Expression::Constant(i)) = &variable_declaration.init {
                InitialValue::Initial(i.clone().into())
            } else if variable_declaration.init.is_none() {
                InitialValue::Tentative
            } else {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::NonConstantInitializerForLocalStaticVariable,
                ));
            };
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::Value(Value::Static(
                    StaticAttr {
                        init: initial_value,
                        global: false,
                    },
                    variable_declaration.var_type.clone(),
                )),
            );
        } else {
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::Value(Value::Local(variable_declaration.var_type.clone())),
            );

            let expr = if let Some(initializer) = variable_declaration.init.as_ref() {
                Some(self.type_check_expression(initializer)?)
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
        } else {
            Type::Long
        }
    }

    fn convert_to(&self, ty: Type, expression: &TCExpression) -> TCExpression {
        let expression_ty = expression.get_type();
        if expression_ty == ty.clone() {
            return expression.clone();
        }
        TCExpression::Cast(ty.clone(), Box::new(expression.clone()))
    }

    fn type_check_expression(
        &self,
        expression: &Expression,
    ) -> Result<TCExpression, CompilerError> {
        match expression {
            Expression::FunctionCall(name, arguments) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    if let Symbol::FunType(_, expected_args, ty) = symbol {
                        if expected_args.len() != arguments.len() {
                            return Err(CompilerError::SemanticAnalysis(
                                SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                            ));
                        }
                        let converted_arguments = arguments.iter().zip(expected_args.iter()).map(|(arg, ty)| {
                            let arg = self.type_check_expression(arg)?;
                            let ty = ty.clone();
                            let converted = self.convert_to(ty, &arg);
                            Ok(converted)
                        }).collect::<Result<Vec<TCExpression>, CompilerError>>()?;


                        let function_call = TCExpression::FunctionCall(name.clone(), converted_arguments.clone(), ty.clone());
                        Ok(function_call) 
                    } else {
                        return Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ));
                    }
                } else {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                    ));
                }         
            }
            Expression::Var(name) => {
                if let Some(existing) = self.symbol_table.get(name) {
                    if existing.is_function() {
                        return Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ));
                    }
                }
                let ty = self.symbol_table.get(name).unwrap().get_type();
                Ok(TCExpression::Var(name.clone(), ty))
            }
            Expression::BinOp(op, left, right) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                match op {
                    BinaryOperator::And | BinaryOperator::Or => Ok(TCExpression::BinOp(
                        op.clone(),
                        Box::new(left),
                        Box::new(right),
                        Type::Int,
                    )),
                    _ => {
                        let ty1 = left.get_type();
                        let ty2 = right.get_type();

                        let ty = Self::get_common_type(ty1, ty2);
                        let left = self.convert_to(ty.clone(), &left);
                        let right = self.convert_to(ty.clone(), &right);
                        match op {
                            BinaryOperator::Add
                            | BinaryOperator::Sub
                            | BinaryOperator::Mul
                            | BinaryOperator::Div
                            | BinaryOperator::Mod => Ok(TCExpression::BinOp(
                                op.clone(),
                                Box::new(left),
                                Box::new(right),
                                ty,
                            )),
                            _ => Ok(TCExpression::BinOp(
                                op.clone(),
                                Box::new(left),
                                Box::new(right),
                                Type::Int,
                            )),
                        }
                    }
                }
            }
            Expression::Unary(UnaryOperator::Not, expression) => {
                let expression = self.type_check_expression(expression)?;
                Ok(TCExpression::Unary(
                    UnaryOperator::Not,
                    Box::new(expression),
                    Type::Int,
                ))
            }
            Expression::Unary(op, expression) => {
                let expression = self.type_check_expression(expression)?;
                let ty = expression.get_type();
                Ok(TCExpression::Unary(op.clone(), Box::new(expression), ty))
            }
            Expression::Assignment(left, right) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                let ty = left.get_type();
                let converted_right = self.convert_to(ty.clone(), &right);

                Ok(TCExpression::Assignment(
                    Box::new(left),
                    Box::new(converted_right),
                    ty,
                ))
            }
            Expression::Conditional(condition, then_expression, else_expression) => {
                let condition = self.type_check_expression(condition)?;
                let then_expression = self.type_check_expression(then_expression)?;
                let else_expression = self.type_check_expression(else_expression)?;
                let ty = then_expression.get_type();
                Ok(TCExpression::Conditional(
                    Box::new(condition),
                    Box::new(then_expression),
                    Box::new(else_expression),
                    ty,
                ))
            }
            Expression::Constant(c) => Ok(TCExpression::Constant(c.clone())),
            Expression::Cast(ty, expr) =>  Ok(self.convert_to(ty.clone(), &self.type_check_expression(expr)?))
        }
    }

    fn type_check_for_init(
        &mut self,
        for_init: &ForInit<Expression>,
    ) -> Result<ForInit<TCExpression>, CompilerError> {
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
        statement: &LLStatement<Expression>,
        containing_function_name : &str
    ) -> Result<LLStatement<TCExpression>, CompilerError> {
        match statement {
            LLStatement::Expression(expression) => Ok(LLStatement::Expression(
                self.type_check_expression(expression)?,
            )),
            LLStatement::Return(expression) => {
                let return_type = self.symbol_table.get(containing_function_name).unwrap().get_type();
                let expression = self.type_check_expression(expression)?;
                let expression_type = expression.get_type();
                let common_type = Self::get_common_type(return_type.clone(), expression_type);
                Ok(LLStatement::Return(self.convert_to(common_type, &expression)))
            }
            LLStatement::If(condition, then_block, else_block) => {
                let condition = self.type_check_expression(condition)?;
                let then_block = self.type_check_statement(then_block.as_ref(), containing_function_name)?;
                let else_block = if let Some(statement) = else_block {
                    Some(Box::new(self.type_check_statement(statement, containing_function_name)?))
                } else {
                    None
                };
                Ok(LLStatement::If(condition, Box::new(then_block), else_block))
            }
            LLStatement::While(condition, block, loop_label) => {
                let condition = self.type_check_expression(condition)?;
                let block = self.type_check_statement(block, containing_function_name)?;
                Ok(LLStatement::While(
                    condition,
                    Box::new(block),
                    loop_label.clone(),
                ))
            }
            LLStatement::DoWhile(body, condition, loop_label) => {
                let body = self.type_check_statement(body, containing_function_name)?;
                let condition = self.type_check_expression(condition)?;
                Ok(LLStatement::DoWhile(
                    Box::new(body),
                    condition,
                    loop_label.clone(),
                ))
            }
            LLStatement::For(for_init, condition, post, block, loop_label) => {
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
                Ok(LLStatement::For(
                    for_init,
                    condition,
                    post,
                    Box::new(block),
                    loop_label.clone(),
                ))
            }
            LLStatement::Compound(block_items) => {
                let mut new_block_items = Vec::new();
                for block_item in block_items {
                    new_block_items.push(self.type_check_block_item(block_item, containing_function_name)?);
                }
                Ok(LLStatement::Compound(new_block_items))
            }
            LLStatement::Null => Ok(LLStatement::Null),
            LLStatement::Continue(loop_label) => Ok(LLStatement::Continue(loop_label.clone())),
            LLStatement::Break(loop_label) => Ok(LLStatement::Break(loop_label.clone())),
        }
    }

    fn type_check_block_item(
        &mut self,
        block_item: &BlockItem<LLStatement<Expression>, Expression>,
        containing_function_name : &str
    ) -> Result<BlockItem<LLStatement<TCExpression>, TCExpression>, CompilerError> {
        match block_item {
            BlockItem::Statement(statement) => {
                Ok(BlockItem::Statement(self.type_check_statement(statement, containing_function_name)?))
            }
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
        function_declaration: &FunctionDeclaration<LLStatement<Expression>, Expression>,
        top_level: bool,
    ) -> Result<FunctionDeclaration<LLStatement<TCExpression>, TCExpression>, CompilerError> {
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
