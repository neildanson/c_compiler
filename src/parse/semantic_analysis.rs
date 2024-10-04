use super::ast::*;
use crate::error::*;
use std::collections::HashMap;

#[derive(Debug)]
struct MapEntry {
    unique_name: String,
    from_current_scope: bool,
    has_external_linkage: bool,
}

impl MapEntry {
    fn new(unique_name: String, from_current_scope: bool, has_external_linkage: bool) -> Self {
        MapEntry {
            unique_name,
            from_current_scope,
            has_external_linkage,
        }
    }
}

impl Clone for MapEntry {
    fn clone(&self) -> Self {
        MapEntry {
            unique_name: self.unique_name.clone(),
            from_current_scope: false, //Is this a bit naughty changing on clone?
            has_external_linkage: self.has_external_linkage,
        }
    }
}

impl From<String> for MapEntry {
    fn from(name: String) -> Self {
        MapEntry {
            unique_name: name,
            from_current_scope: true,
            has_external_linkage: false,
        }
    }
}

#[derive(Default)]
pub struct Analysis {
    counter: i32,
}

impl Analysis {
    pub fn make_label(&mut self) -> String {
        let name = format!("__{}", self.counter);
        self.counter += 1;
        name
    }
}

impl Analysis {
    fn copy_identifier_map(identifier_map: &HashMap<String, MapEntry>) -> HashMap<String, MapEntry> {
        let mut new_map = HashMap::new();
        for (key, value) in identifier_map.iter() {
            new_map.insert(key.clone(), value.clone());
        }
        new_map
    }

    fn resolve_expression(
        expr: &Expression,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<Expression, CompilerError> {
        match expr {
            Expression::Var(name) => {
                let map_entry = identifier_map
                    .get(name)
                    .ok_or(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::VariableNotDeclared(name.clone()),
                    ))?;
                Ok(Expression::Var(map_entry.unique_name.clone()))
            }
            Expression::Unary(op, expr) => {
                let expr = Self::resolve_expression(expr, identifier_map)?;
                Ok(Expression::Unary(op.clone(), Box::new(expr)))
            }
            Expression::BinOp(op, expr1, expr2) => {
                let expr1 = Self::resolve_expression(expr1, identifier_map)?;
                let expr2 = Self::resolve_expression(expr2, identifier_map)?;
                Ok(Expression::BinOp(
                    op.clone(),
                    Box::new(expr1),
                    Box::new(expr2),
                ))
            }
            Expression::Assignment(expr1, expr2) => match expr1.as_ref() {
                Expression::Var(_name) => {
                    let expr1 = Self::resolve_expression(expr1, identifier_map)?;
                    let expr2 = Self::resolve_expression(expr2, identifier_map)?;
                    Ok(Expression::Assignment(Box::new(expr1), Box::new(expr2)))
                }
                _ => Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidLValue,
                )),
            },
            Expression::Constant(_) => Ok(expr.clone()),
            Expression::Conditional(cond, then, els) => {
                let cond = Self::resolve_expression(cond, identifier_map)?;
                let then = Self::resolve_expression(then, identifier_map)?;
                let els = Self::resolve_expression(els, identifier_map)?;
                Ok(Expression::Conditional(
                    Box::new(cond),
                    Box::new(then),
                    Box::new(els),
                ))
            }
            Expression::FunctionCall(name, args) => {
                let ident = identifier_map.get(name);
                match ident {
                    Some(ident) => {
                        let unique_name = ident.unique_name.clone();
                        let args = args
                            .iter()
                            .map(|arg| Self::resolve_expression(arg, identifier_map))
                            .collect::<Result<Vec<Expression>, CompilerError>>()?;
                        Ok(Expression::FunctionCall(unique_name, args))
                    }
                    None => {
                        Err(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                        ))
                    }
                }
            }
        }
    }

    fn resolve_variable_declaration(
        decl: VariableDeclaration,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<VariableDeclaration, CompilerError> {
        if identifier_map.contains_key(&decl.name) && identifier_map[&decl.name].from_current_scope {
            return Err(CompilerError::SemanticAnalysis(
                crate::error::SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
            ));
        }
        let unique_name = format!("{}__{}", decl.name, identifier_map.len());
        identifier_map.insert(decl.name, unique_name.clone().into());
        let init = match decl.value {
            Some(expr) => {
                let expression = Self::resolve_expression(&expr, identifier_map)?;
                Some(expression)
            }
            None => None,
        };
        Ok(VariableDeclaration {
            name: unique_name,
            value: init,
        })
    }

    fn resolve_param(
        param: &Identifier,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<Identifier, CompilerError> {
        if identifier_map.contains_key(param) {
            return Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::VariableAlreadyDeclared(param.clone()),
            ));
        }
        let unique_name = format!("{}__{}", param, identifier_map.len());
        identifier_map.insert(param.clone(), unique_name.clone().into());
        Ok(unique_name)
    }

    fn resolve_function_declaration(
        decl: FunctionDefinition,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<FunctionDefinition, CompilerError> {
        //This is a bit ugly - should be a pattern match

        match identifier_map.get(&decl.name) {
            Some(entry) if entry.from_current_scope => {
            return Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
            ));
            }
            _ => {}
        }

        let unique_name = format!("{}__{}", decl.name, identifier_map.len());
        let map_entry = MapEntry::new(unique_name.clone(), true, true);
        identifier_map.insert(decl.name, map_entry);

        let mut inner_map = Self::copy_identifier_map(identifier_map);

        let parameters = decl
            .parameters
        .iter()
            .map(|param| Self::resolve_param(param, &mut inner_map))
            .collect::<Result<Vec<Identifier>, CompilerError>>()?;

        let body = 
            if let Some(body) = decl.body {
                let body = Self::resolve_block(&body, &mut inner_map)?;
                Some(body)
            } else {
                None
            };


        Ok(FunctionDefinition {
            name: unique_name,
            parameters,
            body,
        })
    }

    fn resolve_block(
        blocks: &[BlockItem],
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<Vec<BlockItem>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Declaration(Declaration::Variable(decl)) => {
                    let decl = Self::resolve_variable_declaration(decl.clone(), identifier_map)?;
                    new_block.push(BlockItem::Declaration(Declaration::Variable(decl)));
                }
                BlockItem::Declaration(Declaration::Function(decl)) => {
                    let decl = Self::resolve_function_declaration(decl.clone(), identifier_map)?;
                    new_block.push(BlockItem::Declaration(Declaration::Function(decl)));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = Self::resolve_statement(stmt, identifier_map)?;
                    new_block.push(BlockItem::Statement(stmt));
                }
                //_ => {
                //    return Err(CompilerError::SemanticAnalysis(
                //        SemanticAnalysisError::InvalidBlockItem,
                //    ))
                //}
            }
        }
        Ok(new_block)
    }

    fn resolve_for_init(
        init: &ForInit,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<ForInit, CompilerError> {
        match init {
            ForInit::InitDeclaration(decl) => {
                let decl = Self::resolve_variable_declaration(decl.clone(), identifier_map)?;
                Ok(ForInit::InitDeclaration(decl))
            }
            ForInit::InitExpression(Some(expr)) => {
                let expr = Self::resolve_expression(expr, identifier_map)?;
                Ok(ForInit::InitExpression(Some(expr)))
            }
            ForInit::InitExpression(None) => Ok(ForInit::InitExpression(None)),
        }
    }

    fn resolve_statement(
        stmt: &Statement,
        identifier_map: &mut HashMap<String, MapEntry>,
    ) -> Result<Statement, CompilerError> {
        match stmt {
            Statement::Return(expr) => {
                let expr = Self::resolve_expression(expr, identifier_map)?;
                Ok(Statement::Return(expr))
            }
            Statement::Expression(expr) => {
                let expr = Self::resolve_expression(expr, identifier_map)?;
                Ok(Statement::Expression(expr))
            }
            Statement::If(expr, then, els) => {
                let expr = Self::resolve_expression(expr, identifier_map)?;
                let then = Self::resolve_statement(then.as_ref(), identifier_map)?;
                let els = match els {
                    Some(els) => Some(Box::new(Self::resolve_statement(els, identifier_map)?)),
                    None => None,
                };
                Ok(Statement::If(expr, Box::new(then), els))
            }
            Statement::Compound(blocks) => {
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let blocks = Self::resolve_block(blocks, &mut new_identifier_map)?;
                Ok(Statement::Compound(blocks))
            }
            Statement::Null => Ok(Statement::Null),
            Statement::For(init, cond, post, body, loop_id) => {
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let for_init = Self::resolve_for_init(init, &mut new_identifier_map)?;
                let cond = cond
                    .clone()
                    .map(|expr| Self::resolve_expression(&expr, &mut new_identifier_map))
                    .transpose()?;
                let post = post
                    .clone()
                    .map(|expr| Self::resolve_expression(&expr, &mut new_identifier_map))
                    .transpose()?;
                let body = Self::resolve_statement(body, &mut new_identifier_map)?;
                Ok(Statement::For(
                    for_init,
                    cond,
                    post,
                    Box::new(body),
                    loop_id.clone(),
                ))
            }
            Statement::DoWhile(body, cond, loop_id) => {
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let body = Self::resolve_statement(body, &mut new_identifier_map)?;
                let cond = Self::resolve_expression(cond, identifier_map)?;
                Ok(Statement::DoWhile(Box::new(body), cond, loop_id.clone()))
            }
            Statement::While(cond, body, loop_id) => {
                let cond = Self::resolve_expression(cond, identifier_map)?;
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let body = Self::resolve_statement(body, &mut new_identifier_map)?;
                Ok(Statement::While(cond, Box::new(body), loop_id.clone()))
            }
            Statement::Break(loop_id) => Ok(Statement::Break(loop_id.clone())),
            Statement::Continue(loop_id) => Ok(Statement::Continue(loop_id.clone())),
            //d => Ok(d.clone()), //TODO: Implement the rest of the statements
        }
    }

    fn resolve_function(function: FunctionDefinition) -> Result<FunctionDefinition, CompilerError> {
        let mut identifier_map = HashMap::new();
        let mut new_body = Vec::new();
        for item in function.body.unwrap() {
            match item {
                BlockItem::Declaration(Declaration::Variable(decl)) => {
                    let decl = Self::resolve_variable_declaration(decl, &mut identifier_map)?;
                    new_body.push(BlockItem::Declaration(Declaration::Variable(decl)));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = Self::resolve_statement(&stmt, &mut identifier_map)?;
                    new_body.push(BlockItem::Statement(stmt));
                }
                _ => {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidBlockItem,
                    ))
                }
            }
        }
        Ok(FunctionDefinition {
            name: function.name,
            parameters: function.parameters,
            body: Some(new_body),
        })
    }

    fn label_block(
        &mut self,
        blocks: &[BlockItem],
        current_label: Option<String>,
    ) -> Result<Vec<BlockItem>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Declaration(decl) => {
                    new_block.push(BlockItem::Declaration(decl.clone()));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = self.label_statement(stmt.clone(), current_label.clone())?;
                    new_block.push(BlockItem::Statement(stmt));
                }
            }
        }
        Ok(new_block)
    }

    fn label_statement(
        &mut self,
        stmt: Statement,
        current_label: Option<String>,
    ) -> Result<Statement, CompilerError> {
        match stmt {
            Statement::Break(_) => {
                if current_label.is_none() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidBreak,
                    ));
                }
                Ok(Statement::Break(current_label))
            }
            Statement::Continue(_) => {
                if current_label.is_none() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidContinue,
                    ));
                }
                Ok(Statement::Continue(current_label))
            }
            Statement::While(condition, body, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::While(condition, Box::new(body), Some(label));
                Ok(new_stmt)
            }
            Statement::DoWhile(body, condition, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::DoWhile(Box::new(body), condition, Some(label));
                Ok(new_stmt)
            }
            Statement::For(init, condition, post, body, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::For(init, condition, post, Box::new(body), Some(label));
                Ok(new_stmt)
            }
            Statement::If(cond, then, els) => {
                let then = self.label_statement(*then, current_label.clone())?;
                let els = match els {
                    Some(els) => Some(Box::new(self.label_statement(*els, current_label.clone())?)),
                    None => None,
                };
                Ok(Statement::If(cond, Box::new(then), els))
            }
            Statement::Compound(block) => {
                let new_block = self.label_block(&block, current_label)?;
                Ok(Statement::Compound(new_block))
            }
            _ => Ok(stmt.clone()),
        }
    }

    fn label_function(
        &mut self,
        function: FunctionDefinition,
    ) -> Result<FunctionDefinition, CompilerError> {
        let new_body = self.label_block(&function.body.unwrap(), None)?;
        Ok(FunctionDefinition {
            name: function.name,
            parameters: function.parameters,
            body: Some(new_body),
        })
    }

    fn verify_statement_labels(stmt: Statement) -> Result<Statement, CompilerError> {
        match stmt {
            Statement::Break(None) => Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::InvalidBreak,
            )),
            Statement::Continue(None) => Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::InvalidContinue,
            )),
            Statement::Compound(block) => {
                let mut new_block = Vec::new();
                for item in block {
                    match item {
                        BlockItem::Declaration(decl) => {
                            new_block.push(BlockItem::Declaration(decl));
                        }
                        BlockItem::Statement(stmt) => {
                            let stmt = Self::verify_statement_labels(stmt)?;
                            new_block.push(BlockItem::Statement(stmt));
                        }
                    }
                }
                Ok(Statement::Compound(new_block))
            }
            Statement::If(cond, then, els) => {
                let then = Self::verify_statement_labels(*then)?;
                let els = match els {
                    Some(els) => Some(Box::new(Self::verify_statement_labels(*els)?)),
                    None => None,
                };
                Ok(Statement::If(cond, Box::new(then), els))
            }
            _ => Ok(stmt.clone()),
        }
    }

    fn verify_function_labels(
        function: FunctionDefinition,
    ) -> Result<FunctionDefinition, CompilerError> {
        let mut new_body = Vec::new();

        if let Some(body) = function.body {
            for item in body {
                match item {
                    BlockItem::Declaration(decl) => {
                        new_body.push(BlockItem::Declaration(decl.clone()));
                    }
                    BlockItem::Statement(stmt) => {
                        let stmt = Self::verify_statement_labels(stmt.clone())?;
                        new_body.push(BlockItem::Statement(stmt));
                    }
                }
            }
            Ok(FunctionDefinition {
                name: function.name,
                parameters: function.parameters,
                body: Some(new_body), 
            })
        } else {
            Ok(FunctionDefinition {
                name: function.name,
                parameters: function.parameters,
                body: None,
            })
        }
    }

    fn semantic_validation_function(
        &mut self,
        function: FunctionDefinition,
    ) -> Result<FunctionDefinition, CompilerError> {
        let function = Self::resolve_function(function)?;
        let function = self.label_function(function)?;
        let function = Self::verify_function_labels(function)?;

        Ok(function)
    }

    pub fn semantic_validation(&mut self, program: Program) -> Result<Program, CompilerError> {
        let mut functions = Vec::new();
        for function in program.functions {
            let function = self.semantic_validation_function(function)?;
            functions.push(function);
        }

        Ok(Program { functions })
    }
}
