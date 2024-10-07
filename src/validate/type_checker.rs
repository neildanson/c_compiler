use std::collections::HashMap;

use crate::{error::*, parse::*};

#[derive(PartialEq)]
enum TypeDefinition {
    Int,
    FunType(usize)
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

#[derive(Default)]
pub (crate) struct TypeChecker {
    symbol_table: HashMap<String, Symbol>,
}

impl TypeChecker {
    fn type_check_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) -> Result<(), CompilerError> {
        self.symbol_table.insert(variable_declaration.name.clone(), Symbol::new(TypeDefinition::Int));
        match &variable_declaration.value {
            Some(initializer) => {
                self.type_check_expression(&initializer)?;
            },
            None => {}
        }
        Ok(())
    }

    fn type_check_expression(&mut self, expression:&Expression) -> Result<(), CompilerError> {
        match expression {
            Expression::FunctionCall(name, arguments) => {
                if let Some(symbol) = self.symbol_table.get(name) {
                    if let TypeDefinition::FunType(expected_args) = symbol.type_definition {
                        if expected_args != arguments.len() {
                            return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::FunctionNotDeclared(name.clone())));
                        }
                    } else {
                        return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::FunctionNotDeclared(name.clone())));
                    }
                } else {
                    return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::FunctionNotDeclared(name.clone())));
                }
                for argument in arguments {
                    self.type_check_expression(argument)?;
                }
            },
            Expression::Var(name) => { 
                if let Some(existing) = self.symbol_table.get(name) {
                    if existing.type_definition != TypeDefinition::Int {
                        return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::VariableUsedAsFunctionName)); 
                    }
                }

            },
            _ => {}
        }
        Ok(())
    }

    fn type_check_for_init(&mut self, for_init:&ForInit) -> Result<(), CompilerError> {
        match for_init {
            ForInit::InitExpression(Some(expression)) => {
                self.type_check_expression(expression)?;
            },
            ForInit::InitExpression(None) => {
            },
            ForInit::InitDeclaration(declaration) => {
                self.type_check_variable_declaration(declaration)?;
            }
        }
        Ok(())
    }

    fn type_check_statement(&mut self, statement: &Statement) -> Result<(), CompilerError> {
        match statement {
            Statement::Expression(expression) => {
                self.type_check_expression(expression)?;
            },
            Statement::Return(expression) => {
                self.type_check_expression(expression)?;
            },
            Statement::If (condition, then_block, else_block) => {
                self.type_check_expression(condition)?;
                self.type_check_statement(then_block.as_ref())?;
                if let Some(statement) = else_block {
                    self.type_check_statement(statement)?;   
                }
            },
            Statement::While (condition, block, _) => {
                self.type_check_expression(condition)?;
                self.type_check_statement(block)?;
                
            },
            Statement::DoWhile(body, condition , _ ) => {
                self.type_check_statement(body)?;
                self.type_check_expression(condition)?;
            },
            Statement::For (for_init, condition, post, block, _) => {
                self.type_check_for_init(for_init)?;
                if let Some(condition) = condition {
                    self.type_check_expression(condition)?;
                }
                if let Some(post) = post {
                    self.type_check_expression(post)?;
                }
                self.type_check_statement(block)?;
                
            },
            Statement::Compound(block_items) => {
                for block_item in block_items {
                    self.type_check_block_item(block_item)?;
                }
            },
            Statement::Null => {},
            Statement::Continue(_) => {},
            Statement::Break(_) => {},
        }
        Ok(())
    }

    fn type_check_block_item(&mut self, block_item: &BlockItem) -> Result<(), CompilerError> {
        match block_item {
            BlockItem::Statement(statement) => {
                self.type_check_statement(statement)?;
            },
            BlockItem::Declaration(declaration) => {
                match declaration {
                    Declaration::Variable(variable_declaration) => {
                        self.type_check_variable_declaration(variable_declaration)?;
                    },
                    Declaration::Function(function_declaration) => {
                        self.type_check_function_declaration(function_declaration)?;
                    }
                }
            }
        }
        Ok(())
    }

    pub fn type_check_function_declaration(&mut self, function_declaration: &FunctionDefinition) -> Result<FunctionDefinition, CompilerError> {
        let fun_type = TypeDefinition::FunType(function_declaration.parameters.len());
        let has_body = function_declaration.body.is_some();
        let mut already_defined = false;

        if let Some(old_decl) = self.symbol_table.get(&function_declaration.name) {
            if old_decl.type_definition != fun_type {
                return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::IncompatibleFunctionDeclarations));
            }
            already_defined = old_decl.is_defined;
            if already_defined && has_body {
                return Err(CompilerError::SemanticAnalysis(SemanticAnalysisError::FunctionAlreadyDeclared(function_declaration.name.clone())));
            }
        }
        self.symbol_table.insert(function_declaration.name.clone(), {
            Symbol {
                type_definition: fun_type,
                is_defined: has_body || already_defined,
            }
        });
        if let Some(body) = function_declaration.body.as_ref() {
            for param in &function_declaration.parameters {
                self.symbol_table.insert(param.clone(), Symbol::new(TypeDefinition::Int));
            }
            for block_item in body {
                self.type_check_block_item(block_item)?;
            }
            
        }
        Ok(function_declaration.clone())
    }

}

