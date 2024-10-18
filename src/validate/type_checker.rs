use std::collections::HashMap;

use crate::{error::*, parse::*};

#[derive(PartialEq)]
enum TypeDefinition {
    Int,
    FunType(usize),
}

#[derive(PartialEq)]
struct Symbol {
    type_definition: TypeDefinition,
    is_defined: bool,
}

impl Symbol {
    fn new(type_definition: TypeDefinition) -> Self {
        Symbol {
            type_definition,
            is_defined: false,
        }
    }
}

#[derive(PartialEq, Clone)]
enum InitialValue { 
    Tentative, 
    Initial(i32),
    NoInitializer
}

impl InitialValue { 
    fn is_constant(&self) -> bool {
        match self {
            InitialValue::Initial(_) => true,
            _ => false,
        }
    }
    fn is_tentative(&self) -> bool {
        match self {
            InitialValue::Tentative => true,
            _ => false,
        }
    }
}

enum IdentifierAttributes{
    FunAttr{ defined : bool, global : bool }, 
    StaticAttr { init : InitialValue, global : bool },
    LocalAttr,
}

impl IdentifierAttributes {
    fn is_global(&self) -> bool {
        match self {
            IdentifierAttributes::FunAttr { global, .. } => *global,
            IdentifierAttributes::StaticAttr { global, .. } => *global,
            IdentifierAttributes::LocalAttr => false,
        }
    }

    fn init(&self) -> InitialValue {
        match self {
            IdentifierAttributes::StaticAttr { init, .. } => init.clone(),
            _ => InitialValue::NoInitializer,
        }
    }
}

#[derive(Default)]
pub(crate) struct TypeChecker {
    symbol_table: HashMap<String, (Symbol, IdentifierAttributes)>,
}

impl TypeChecker {
    pub fn type_check_file_scope_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        let mut initial_value = if let Some(Expression::Constant(i)) = variable_declaration.value {
            InitialValue::Initial(i)
        } else if variable_declaration.storage_class == Some(StorageClass::Extern) {
            InitialValue::NoInitializer
        } else {
            InitialValue::Tentative
        };

        let mut global = variable_declaration.storage_class != Some(StorageClass::Static);
        
        if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
            if old_decl.0.type_definition != TypeDefinition::Int {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::FunctionRedclaredAsVariable(
                        variable_declaration.name.clone(),
                    ),
                ));
            }
            if variable_declaration.storage_class == Some(StorageClass::Extern) {
                global = old_decl.1.is_global();
            } else if old_decl.1.is_global() != global {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::ConflictingVariableLinkage(
                        variable_declaration.name.clone(),
                    ),
                ));
            }

            if let InitialValue::Initial(_) = old_decl.1.init()   {
                if let InitialValue::Initial(_) = initial_value {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::ConflictingFileScopeVariableDefinitions(
                            variable_declaration.name.clone(),
                        ),
                    ));
                } else {
                    initial_value = old_decl.1.init();
                }
            } else {
                if !initial_value.is_constant() && old_decl.1.init().is_tentative()  {
                    initial_value = InitialValue::Tentative;
                }
            }
        }

        let attrs = IdentifierAttributes::StaticAttr {
            init: initial_value,
            global,
        };

        self.symbol_table.insert(variable_declaration.name.clone(), (Symbol::new(TypeDefinition::Int), attrs));

        Ok(())
    }


    fn type_check_local_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<(), CompilerError> {
        if variable_declaration.storage_class == Some(StorageClass::Extern) {
            if variable_declaration.value.is_some() {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidLValue, //Wrong wrror
                ));
            }
            if let Some(old_decl) = self.symbol_table.get(&variable_declaration.name) {
                if old_decl.0.type_definition != TypeDefinition::Int {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionRedclaredAsVariable(
                            variable_declaration.name.clone(),
                        ),
                    ));
                }
            } else {
                self.symbol_table.insert(
                    variable_declaration.name.clone(),
                    (Symbol::new(TypeDefinition::Int), IdentifierAttributes::StaticAttr { init: InitialValue::NoInitializer, global: true }),
                );
            }
        } else if variable_declaration.storage_class == Some(StorageClass::Static) {
            let initial_value = if let Some(Expression::Constant(i)) = variable_declaration.value {
                InitialValue::Initial(i)
            } else if variable_declaration.value.is_none() {
                InitialValue::Initial(0)
            } else {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidLValue, //TODO
                ));
            };
            self.symbol_table.insert(variable_declaration.name.clone(), (Symbol::new(TypeDefinition::Int), IdentifierAttributes::StaticAttr { init: initial_value, global: false }));
        } else {
            self.symbol_table.insert(
                variable_declaration.name.clone(),
                (Symbol::new(TypeDefinition::Int), IdentifierAttributes::LocalAttr),
            );
            if let Some(initializer) = variable_declaration.value.as_ref() {
                self.type_check_expression(initializer)?;
            }
        }
        Ok(())
    }

    fn type_check_expression(&mut self, expression: &Expression) -> Result<(), CompilerError> {
        match expression {
            Expression::FunctionCall(name, arguments) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    if let TypeDefinition::FunType(expected_args) = symbol.0.type_definition {
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
                    if existing.0.type_definition != TypeDefinition::Int {
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
                        SemanticAnalysisError::InvalidLValue, //TODO
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
                    self.type_check_function_declaration(function_declaration)?;
                }
            },
        }
        Ok(())
    }

    pub fn type_check_function_declaration(
        &mut self,
        function_declaration: &FunctionDeclaration,
    ) -> Result<(), CompilerError> {
        let fun_type = TypeDefinition::FunType(function_declaration.parameters.len());
        let has_body = function_declaration.body.is_some();
        let mut already_defined = false;
        let mut global = function_declaration.storage_class != Some(StorageClass::Static);
        if let Some(old_decl) = self.symbol_table.get(&function_declaration.name) {
            if old_decl.0.type_definition != fun_type {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::IncompatibleFunctionDeclarations,
                ));
            }
            already_defined = old_decl.0.is_defined;
            if already_defined && has_body {
                println!("Function {} already defined", function_declaration.name);
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::FunctionAlreadyDeclared(
                        function_declaration.name.clone(),
                    ),
                ));
            }

            if old_decl.1.is_global() && function_declaration.storage_class == Some(StorageClass::Static) {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::StaticFunctionDeclarationFollowsNonStatic(
                        function_declaration.name.clone(),
                    ),
                ));
            }
            global = old_decl.1.is_global();
        }
        let attrs = IdentifierAttributes::FunAttr {
            defined: has_body || already_defined,
            global,
        };
        self.symbol_table
            .insert(function_declaration.name.clone(), {
                (Symbol {
                    type_definition: fun_type,
                    is_defined: has_body || already_defined,
                },
                attrs)
            });
        if let Some(body) = function_declaration.body.as_ref() {
            for param in &function_declaration.parameters {
                self.symbol_table
                    .insert(param.clone(), (Symbol::new(TypeDefinition::Int), IdentifierAttributes::LocalAttr));
            }
            for block_item in body {
                self.type_check_block_item(block_item)?;
            }
        }
        Ok(())
    }
}
