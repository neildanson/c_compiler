use crate::error::*;
use crate::parse::ast::*;
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct MapEntry {
    unique_name: Identifier,
    from_current_scope: bool,
    has_external_linkage: bool,
}

impl MapEntry {
    pub(crate) fn new(
        unique_name: Identifier,
        from_current_scope: bool,
        has_external_linkage: bool,
    ) -> Self {
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
pub struct IdentifierResolution;

impl IdentifierResolution {
    fn copy_identifier_map(
        identifier_map: &HashMap<Identifier, MapEntry>,
    ) -> HashMap<Identifier, MapEntry> {
        let mut new_map = HashMap::new();
        for (key, value) in identifier_map.iter() {
            new_map.insert(key.clone(), value.clone());
        }
        new_map
    }

    fn resolve_expression(
        &self,
        expr: &Expression,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
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
                let expr = self.resolve_expression(expr, identifier_map)?;
                Ok(Expression::Unary(op.clone(), Box::new(expr)))
            }
            Expression::BinOp(op, expr1, expr2) => {
                let expr1 = self.resolve_expression(expr1, identifier_map)?;
                let expr2 = self.resolve_expression(expr2, identifier_map)?;
                Ok(Expression::BinOp(
                    op.clone(),
                    Box::new(expr1),
                    Box::new(expr2),
                ))
            }
            Expression::Assignment(expr1, expr2) => match expr1.as_ref() {
                Expression::Var(_name) => {
                    let expr1 = self.resolve_expression(expr1, identifier_map)?;
                    let expr2 = self.resolve_expression(expr2, identifier_map)?;
                    Ok(Expression::Assignment(Box::new(expr1), Box::new(expr2)))
                }
                _ => Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidLValue,
                )),
            },
            Expression::Constant(_) => Ok(expr.clone()),
            Expression::Conditional(cond, then, els) => {
                let cond = self.resolve_expression(cond, identifier_map)?;
                let then = self.resolve_expression(then, identifier_map)?;
                let els = self.resolve_expression(els, identifier_map)?;
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
                            .map(|arg| self.resolve_expression(arg, identifier_map))
                            .collect::<Result<Vec<Expression>, CompilerError>>()?;
                        Ok(Expression::FunctionCall(unique_name, args))
                    }
                    None => Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                    )),
                }
            }
        }
    }

    fn resolve_variable_declaration(
        &self,
        decl: VariableDeclaration,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<VariableDeclaration, CompilerError> {
        match identifier_map.get(&decl.name) {
            Some(entry) if entry.from_current_scope => {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
                ));
            }
            _ => {}
        }
        let unique_name = format!("{}__{}", decl.name, identifier_map.len());
        identifier_map.insert(decl.name, unique_name.clone().into());
        let init = match decl.value {
            Some(expr) => {
                let expression = self.resolve_expression(&expr, identifier_map)?;
                Some(expression)
            }
            None => None,
        };
        Ok(VariableDeclaration {
            name: unique_name,
            value: init,
        })
    }

    //This is very similar to resolve_variable_declaration
    //Consider refactoring
    fn resolve_param(
        param: Identifier,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<Identifier, CompilerError> {
        if identifier_map.contains_key(&param) && identifier_map[&param].from_current_scope {
            return Err(CompilerError::SemanticAnalysis(
                crate::error::SemanticAnalysisError::VariableAlreadyDeclared(param.clone()),
            ));
        }
        let unique_name = format!("{}__{}", param, identifier_map.len());
        identifier_map.insert(param.clone(), unique_name.clone().into());
        Ok(unique_name)
    }

    pub fn resolve_function_declaration(
        &self,
        decl: FunctionDefinition,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
        nested: bool,
    ) -> Result<FunctionDefinition, CompilerError> {
        match identifier_map.get(&decl.name) {
            // this never hits, but matches the book when checking has external linkage
            // more tests pass when this is commented, but it's not clear why
            Some(entry) => {
                if entry.from_current_scope
                /*&& !entry.has_external_linkage*/
                {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionAlreadyDeclared(decl.name),
                    ));
                }
            }
            _ => {}
        }

        let unique_name = decl.name.clone();
        let map_entry = MapEntry::new(unique_name.clone(), true, true);
        identifier_map.insert(decl.name, map_entry);

        let mut inner_map = Self::copy_identifier_map(identifier_map);

        let parameters = decl
            .parameters
            .iter()
            .map(|param| Self::resolve_param(param.clone(), &mut inner_map))
            .collect::<Result<Vec<Identifier>, CompilerError>>()?;

        let body = if let Some(body) = decl.body {
            if nested {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::NestedFunction,
                ));
            }
            let body = self.resolve_block(&body, &mut inner_map)?;
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
        &self,
        blocks: &[BlockItem],
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<Vec<BlockItem>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Declaration(Declaration::Variable(decl)) => {
                    let decl = self.resolve_variable_declaration(decl.clone(), identifier_map)?;
                    new_block.push(BlockItem::Declaration(Declaration::Variable(decl)));
                }
                BlockItem::Declaration(Declaration::Function(decl)) => {
                    let decl =
                        self.resolve_function_declaration(decl.clone(), identifier_map, true)?;
                    new_block.push(BlockItem::Declaration(Declaration::Function(decl)));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = self.resolve_statement(stmt, identifier_map)?;
                    new_block.push(BlockItem::Statement(stmt));
                } //_ => {
                  //    return Err(CompilerError::SemanticAnalysis(
                  //        SemanticAnalysisError::InvalidBlockItem,
                  //    ))
                  //}
            }
        }
        Ok(new_block)
    }

    fn resolve_for_init(
        &self,
        init: &ForInit,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<ForInit, CompilerError> {
        match init {
            ForInit::InitDeclaration(decl) => {
                let decl = self.resolve_variable_declaration(decl.clone(), identifier_map)?;
                Ok(ForInit::InitDeclaration(decl))
            }
            ForInit::InitExpression(Some(expr)) => {
                let expr = self.resolve_expression(expr, identifier_map)?;
                Ok(ForInit::InitExpression(Some(expr)))
            }
            ForInit::InitExpression(None) => Ok(ForInit::InitExpression(None)),
        }
    }

    fn resolve_statement(
        &self,
        stmt: &Statement,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<Statement, CompilerError> {
        match stmt {
            Statement::Return(expr) => {
                let expr = self.resolve_expression(expr, identifier_map)?;
                Ok(Statement::Return(expr))
            }
            Statement::Expression(expr) => {
                let expr = self.resolve_expression(expr, identifier_map)?;
                Ok(Statement::Expression(expr))
            }
            Statement::If(expr, then, els) => {
                let expr = self.resolve_expression(expr, identifier_map)?;
                let then = self.resolve_statement(then.as_ref(), identifier_map)?;
                let els = match els {
                    Some(els) => Some(Box::new(self.resolve_statement(els, identifier_map)?)),
                    None => None,
                };
                Ok(Statement::If(expr, Box::new(then), els))
            }
            Statement::Compound(blocks) => {
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let blocks = self.resolve_block(blocks, &mut new_identifier_map)?;
                Ok(Statement::Compound(blocks))
            }
            Statement::Null => Ok(Statement::Null),
            Statement::For(init, cond, post, body, loop_id) => {
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let for_init = self.resolve_for_init(init, &mut new_identifier_map)?;
                let cond = cond
                    .clone()
                    .map(|expr| self.resolve_expression(&expr, &mut new_identifier_map))
                    .transpose()?;
                let post = post
                    .clone()
                    .map(|expr| self.resolve_expression(&expr, &mut new_identifier_map))
                    .transpose()?;
                let body = self.resolve_statement(body, &mut new_identifier_map)?;
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
                let body = self.resolve_statement(body, &mut new_identifier_map)?;
                let cond = self.resolve_expression(cond, identifier_map)?;
                Ok(Statement::DoWhile(Box::new(body), cond, loop_id.clone()))
            }
            Statement::While(cond, body, loop_id) => {
                let cond = self.resolve_expression(cond, identifier_map)?;
                let mut new_identifier_map = Self::copy_identifier_map(identifier_map);
                let body = self.resolve_statement(body, &mut new_identifier_map)?;
                Ok(Statement::While(cond, Box::new(body), loop_id.clone()))
            }
            Statement::Break(loop_id) => Ok(Statement::Break(loop_id.clone())),
            Statement::Continue(loop_id) => Ok(Statement::Continue(loop_id.clone())),
            //d => Ok(d.clone()), //TODO: Implement the rest of the statements
        }
    }
}
