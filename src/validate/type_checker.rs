use std::collections::HashMap;

use crate::{error::*, parse::*};

#[derive(PartialEq, Debug, Clone)]
enum TypeDefinition {
    Type(Type),
    FunType(usize),
}

impl TypeDefinition {
    fn get_type(&self) -> Option<Type> {
        match self {
            TypeDefinition::Type(ty) => Some(ty.clone()),
            TypeDefinition::FunType(_) => None,
        }
    }

    fn is_function(&self) -> bool {
        matches!(self, TypeDefinition::FunType(_))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    type_definition: TypeDefinition,
    pub attributes: IdentifierAttributes,
}

impl Symbol {
    fn new(type_definition: TypeDefinition, attributes: IdentifierAttributes) -> Self {
        Symbol {
            type_definition,
            attributes,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum StaticInit {
    IntInit(i32),
    LongInit(i64)
}

impl StaticInit {
    //TODO: Remove this functionn
    pub fn i32(&self) -> i32 {
        match self {
            StaticInit::IntInit(val) => *val,
            _ => panic!("Invalid conversion")
        }
    }
}


#[derive(PartialEq, Clone, Debug)]
pub enum InitialValue {
    Tentative,
    Initial(StaticInit),
    NoInitializer,
}

impl InitialValue {
    fn is_constant(&self) -> bool {
        matches!(self, InitialValue::Initial(_))
    }
    fn is_tentative(&self) -> bool {
        matches!(self, InitialValue::Tentative)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunAttr {
    defined: bool,
    global: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct StaticAttr {
    pub init: InitialValue,
    pub global: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub enum IdentifierAttributes {
    Fun(FunAttr),
    Static(StaticAttr),
    Local,
}

impl IdentifierAttributes {
    pub fn is_global(&self) -> bool {
        match self {
            IdentifierAttributes::Fun(fun_attr) => fun_attr.global,
            IdentifierAttributes::Static(static_attr) => static_attr.global,
            IdentifierAttributes::Local => false,
        }
    }

    pub fn init(&self) -> InitialValue {
        match self {
            IdentifierAttributes::Static(static_attr) => static_attr.init.clone(),
            _ => InitialValue::NoInitializer,
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, IdentifierAttributes::Static { .. })
    }
}

#[derive(Default)]
pub(crate) struct TypeChecker {
    pub symbol_table: HashMap<String, Symbol>,
}

impl TypeChecker {
    pub fn type_check_file_scope_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
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
            if old_decl.type_definition.is_function()  {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::FunctionRedclaredAsVariable(
                        variable_declaration.name.clone(),
                    ),
                ));
            }
            if variable_declaration.storage_class == Some(StorageClass::Extern) {
                global = old_decl.attributes.is_global();
            } else if old_decl.attributes.is_global() != global {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ConflictingVariableLinkage(
                        variable_declaration.name.clone(),
                    ),
                ));
            }

            if let InitialValue::Initial(_) = old_decl.attributes.init() {
                if let InitialValue::Initial(_) = initial_value {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::ConflictingFileScopeVariableDefinitions(
                            variable_declaration.name.clone(),
                        ),
                    ));
                } else {
                    initial_value = old_decl.attributes.init();
                }
            } else if !initial_value.is_constant() && old_decl.attributes.init().is_tentative() {
                initial_value = InitialValue::Tentative;
            }
        }

        let attrs = IdentifierAttributes::Static(StaticAttr {
            init: initial_value,
            global,
        });

        self.symbol_table.insert(
            variable_declaration.name.clone(),
            Symbol::new(TypeDefinition::Type(variable_declaration.var_type.clone()), attrs),
        );

        Ok(())
    }

    fn type_check_local_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<VariableDeclaration, CompilerError> {
        if variable_declaration.storage_class == Some(StorageClass::Extern) {
            if variable_declaration.init.is_some() {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ExternVariableCannotHaveInitializer, //Wrong wrror
                ));
            }
            if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
                if old_decl.type_definition.is_function() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionRedclaredAsVariable(
                            variable_declaration.name.clone(),
                        ),
                    ));
                }
            } else {
                self.symbol_table.insert(
                    variable_declaration.name.clone(),
                    Symbol::new(
                        TypeDefinition::Type(variable_declaration.var_type.clone()),
                        IdentifierAttributes::Static(StaticAttr {
                            init: InitialValue::NoInitializer,
                            global: true,
                        }),
                    ),
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
                Symbol::new(
                    TypeDefinition::Type(variable_declaration.var_type.clone()),
                    IdentifierAttributes::Static(StaticAttr {
                        init: initial_value,
                        global: false,
                    }),
                ),
            );
        } else {
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::new(TypeDefinition::Type(variable_declaration.var_type.clone()), IdentifierAttributes::Local),
            );
            if let Some(initializer) = variable_declaration.init.as_ref() {
                self.type_check_expression(initializer)?;
            }
        }
        Ok(variable_declaration.clone())
    }

    fn get_common_type(ty1 : Type, ty2: Type) -> Type {
        if ty1 == ty2 {
            return ty1;
        } else {
            Type::Long
        }
    }

    fn convert_to(&self, ty: Type, expression: &Expression) -> Expression {
        let expression_ty = expression.get_type();
        if expression_ty == Some(ty.clone()) {
            return expression.clone();
        }
        let cast = Expression::Cast(ty.clone(), Box::new(expression.clone()));
        cast
    }

    fn type_check_expression(&mut self, expression: &Expression) -> Result<Expression, CompilerError> {
        match expression {
            Expression::FunctionCall(name, arguments, _) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    if let TypeDefinition::FunType(expected_args) = symbol.type_definition {
                        if expected_args != arguments.len() {
                            return Err(CompilerError::SemanticAnalysis(
                                SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                            ));
                        }
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
                for argument in arguments {
                    self.type_check_expression(argument)?;
                }
                Ok(expression.clone()) //
            }
            Expression::Var(name, _ty) => {
                if let Some(existing) = self.symbol_table.get(name) {
                    if existing.type_definition.is_function() {
                        return Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ));
                    }
                }
                let ty = self.symbol_table.get(name).map_or(None, |t| t.type_definition.get_type());
                Ok(Expression::Var(name.clone(), ty)) 
            }
            Expression::BinOp(op, left, right, _) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                let ty = left.get_type();
                Ok(Expression::BinOp(op.clone(), Box::new(left), Box::new(right), ty))
            }
            Expression::Unary(op, expression, _) => {
                let expression = self.type_check_expression(expression)?;
                let ty = expression.get_type();
                Ok(Expression::Unary(op.clone(), Box::new(expression), ty))
            }
            Expression::Assignment(left, right, _) => {
                let left = self.type_check_expression(left)?;
                let right = self.type_check_expression(right)?;
                //if left.get_type() != right.get_type() {
                //    return Err(CompilerError::SemanticAnalysis(
                //        SemanticAnalysisError::IncompatibleTypesInAssignment,
                //    ));
                //}
                let ty = left.get_type();
                Ok(Expression::Assignment(Box::new(left), Box::new(right), ty))
            }
            Expression::Conditional(condition, then_expression, else_expression, _) => {
                let condition = self.type_check_expression(condition)?;
                let then_expression = self.type_check_expression(then_expression)?;
                let else_expression = self.type_check_expression(else_expression)?;
                //if then_expression.get_type() != else_expression.get_type() {
                //    return Err(CompilerError::SemanticAnalysis(
                //        SemanticAnalysisError::IncompatibleTypesInConditional,
                //    ));
                //}
                let ty = then_expression.get_type();
                Ok(Expression::Conditional(
                    Box::new(condition),
                    Box::new(then_expression),
                    Box::new(else_expression),
                    ty,
                ))
            }
            Expression::Constant(_) => Ok(expression.clone()),
            Expression::Cast(_, _) => Ok(expression.clone()),
        }
    }

    fn type_check_for_init(&mut self, for_init: &ForInit) -> Result<ForInit, CompilerError> {
        match for_init {
            ForInit::InitExpression(Some(expression)) => {
                Ok(ForInit::InitExpression(Some(self.type_check_expression(expression)?)))
            }
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

    fn type_check_statement(&mut self, statement: &Statement) -> Result<Statement, CompilerError> {
        match statement {
            Statement::Expression(expression) => {
                Ok(Statement::Expression(self.type_check_expression(expression)?))
            }
            Statement::Return(expression) => {
                Ok(Statement::Return(self.type_check_expression(expression)?))
            }
            Statement::If(condition, then_block, else_block) => {
                let condition = self.type_check_expression(condition)?;
                let then_block = self.type_check_statement(then_block.as_ref())?;
                let else_block = if let Some(statement) = else_block {
                    Some(Box::new(self.type_check_statement(statement)?))
                } else {
                    None
                };
                Ok(Statement::If(condition, Box::new(then_block), else_block))
            }
            Statement::While(condition, block, loop_label) => {
                let condition = self.type_check_expression(condition)?;
                let block = self.type_check_statement(block)?;
                Ok(Statement::While(condition, Box::new(block), loop_label.clone()))
            }
            Statement::DoWhile(body, condition, loop_label) => {
                let body = self.type_check_statement(body)?;
                let condition = self.type_check_expression(condition)?;
                Ok(Statement::DoWhile(Box::new(body), condition, loop_label.clone()))
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
                let block = self.type_check_statement(block)?;
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
                    new_block_items.push(self.type_check_block_item(block_item)?);
                }
                Ok(Statement::Compound(new_block_items))
            }
            Statement::Null => Ok(Statement::Null),
            Statement::Continue(loop_label) => Ok(Statement::Continue(loop_label.clone())),
            Statement::Break(loop_label) => Ok(Statement::Break(loop_label.clone())),
        }
    }

    fn type_check_block_item(&mut self, block_item: &BlockItem) -> Result<BlockItem, CompilerError> {
        match block_item {
            BlockItem::Statement(statement) => {
                Ok(BlockItem::Statement(self.type_check_statement(statement)?))
            }
            BlockItem::Declaration(declaration) => match declaration {
                Declaration::Variable(variable_declaration) => {
                    Ok(BlockItem::Declaration(Declaration::Variable(self.type_check_local_variable_declaration(variable_declaration)?)))
                }
                Declaration::Function(function_declaration) => {
                    Ok(BlockItem::Declaration(Declaration::Function(self.type_check_function_declaration(function_declaration, false)?)))
                }
            },
        }
    }

    pub fn type_check_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration,
        top_level: bool,
    ) -> Result<FunctionDeclaration, CompilerError> {
        let fun_type = TypeDefinition::FunType(function_declaration.parameters.len());
        let has_body = function_declaration.body.is_some();
        let mut already_defined = false;
        let mut global = function_declaration.storage_class != Some(StorageClass::Static);
        if let Some(old_decl) = self.symbol_table.get(&function_declaration.name) {
            if let IdentifierAttributes::Fun(old_fun_attr) = old_decl.attributes.clone() {
                if old_decl.type_definition != fun_type {
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
                    SemanticAnalysisError::IncompatibleFunctionDeclarations,
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

        let attrs = IdentifierAttributes::Fun(FunAttr {
            defined: has_body || already_defined,
            global,
        });
        self.symbol_table
            .insert(function_declaration.name.clone(), {
                Symbol::new(fun_type, attrs)
            });

        let new_body =     
            if let Some(body) = function_declaration.body.as_ref() {
            for (ty, param) in &function_declaration.parameters {
                self.symbol_table.insert(
                    param.clone(),
                    Symbol::new(TypeDefinition::Type(ty.clone()), IdentifierAttributes::Local),
                );
            }
            let mut new_body = Vec::new();
            for block_item in body {
                new_body.push(self.type_check_block_item(block_item)?);
            }
            Some(new_body)
        } else {
            None
        };
        let function_declaration = FunctionDeclaration {
            body : new_body,
            ..function_declaration.clone()
        };

        Ok(function_declaration) 
    }
}
