use std::collections::HashMap;

use crate::{error::*, parse::*};

#[derive(PartialEq, Debug, Clone)]
enum TypeDefinition {
    Int,
    FunType(usize),
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
pub enum InitialValue {
    Tentative,
    Initial(i32),
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
            if old_decl.type_definition != TypeDefinition::Int {
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
            Symbol::new(TypeDefinition::Int, attrs),
        );

        Ok(())
    }

    fn type_check_local_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        if variable_declaration.storage_class == Some(StorageClass::Extern) {
            if variable_declaration.init.is_some() {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ExternVariableCannotHaveInitializer, //Wrong wrror
                ));
            }
            if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
                if old_decl.type_definition != TypeDefinition::Int {
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
                        TypeDefinition::Int,
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
                InitialValue::Initial(0)
            } else {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::NonConstantInitializerForLocalStaticVariable,
                ));
            };
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::new(
                    TypeDefinition::Int,
                    IdentifierAttributes::Static(StaticAttr {
                        init: initial_value,
                        global: false,
                    }),
                ),
            );
        } else {
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                Symbol::new(TypeDefinition::Int, IdentifierAttributes::Local),
            );
            if let Some(initializer) = variable_declaration.init.as_ref() {
                self.type_check_expression(initializer)?;
            }
        }
        Ok(())
    }

    fn type_check_expression(&mut self, expression: &Expression) -> Result<(), CompilerError> {
        match expression {
            Expression::FunctionCall(name, arguments) => {
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
            }
            Expression::Var(name) => {
                if let Some(existing) = self.symbol_table.get(name) {
                    if existing.type_definition != TypeDefinition::Int {
                        return Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableUsedAsFunctionName,
                        ));
                    }
                }
            }
            Expression::BinOp(_, left, right) => {
                self.type_check_expression(left)?;
                self.type_check_expression(right)?;
            }
            Expression::Unary(_, expression) => {
                self.type_check_expression(expression)?;
            }
            Expression::Assignment(left, right) => {
                self.type_check_expression(left)?;
                self.type_check_expression(right)?;
            }
            Expression::Conditional(condition, then_expression, else_expression) => {
                self.type_check_expression(condition)?;
                self.type_check_expression(then_expression)?;
                self.type_check_expression(else_expression)?;
            }
            Expression::Constant(_) => {}
            Expression::Cast(_, _) => todo!("Type Checking Not done for Long/Cast"),
        }
        Ok(())
    }

    fn type_check_for_init(&mut self, for_init: &ForInit) -> Result<(), CompilerError> {
        match for_init {
            ForInit::InitExpression(Some(expression)) => {
                self.type_check_expression(expression)?;
            }
            ForInit::InitExpression(None) => {}
            ForInit::InitDeclaration(declaration) => {
                if declaration.storage_class.is_some() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::StaticValueNotValidInForLoopInitializer,
                    ));
                }
                self.type_check_local_variable_declaration(declaration)?;
            }
        }
        Ok(())
    }

    fn type_check_statement(&mut self, statement: &Statement) -> Result<(), CompilerError> {
        match statement {
            Statement::Expression(expression) => {
                self.type_check_expression(expression)?;
            }
            Statement::Return(expression) => {
                self.type_check_expression(expression)?;
            }
            Statement::If(condition, then_block, else_block) => {
                self.type_check_expression(condition)?;
                self.type_check_statement(then_block.as_ref())?;
                if let Some(statement) = else_block {
                    self.type_check_statement(statement)?;
                }
            }
            Statement::While(condition, block, _) => {
                self.type_check_expression(condition)?;
                self.type_check_statement(block)?;
            }
            Statement::DoWhile(body, condition, _) => {
                self.type_check_statement(body)?;
                self.type_check_expression(condition)?;
            }
            Statement::For(for_init, condition, post, block, _) => {
                self.type_check_for_init(for_init)?;
                if let Some(condition) = condition {
                    self.type_check_expression(condition)?;
                }
                if let Some(post) = post {
                    self.type_check_expression(post)?;
                }
                self.type_check_statement(block)?;
            }
            Statement::Compound(block_items) => {
                for block_item in block_items {
                    self.type_check_block_item(block_item)?;
                }
            }
            Statement::Null => {}
            Statement::Continue(_) => {}
            Statement::Break(_) => {}
        }
        Ok(())
    }

    fn type_check_block_item(&mut self, block_item: &BlockItem) -> Result<(), CompilerError> {
        match block_item {
            BlockItem::Statement(statement) => {
                self.type_check_statement(statement)?;
            }
            BlockItem::Declaration(declaration) => match declaration {
                Declaration::Variable(variable_declaration) => {
                    self.type_check_local_variable_declaration(variable_declaration)?;
                }
                Declaration::Function(function_declaration) => {
                    self.type_check_function_declaration(function_declaration, false)?;
                }
            },
        }
        Ok(())
    }

    pub fn type_check_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration,
        top_level: bool,
    ) -> Result<(), CompilerError> {
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
        if let Some(body) = function_declaration.body.as_ref() {
            for param in &function_declaration.parameters {
                self.symbol_table.insert(
                    param.clone(),
                    Symbol::new(TypeDefinition::Int, IdentifierAttributes::Local),
                );
            }
            for block_item in body {
                self.type_check_block_item(block_item)?;
            }
        }
        Ok(())
    }
}
