use crate::error::*;
use super::ast::*;   
use std::collections::HashMap;

#[derive(Debug)]
struct MapEntry {
    unique_name: String,
    from_current_scope: bool,   
}

impl Clone for MapEntry {
    fn clone(&self) -> Self {
        MapEntry {
            unique_name: self.unique_name.clone(),
            from_current_scope: false, //Is this a bit naughty changing on clone?
        }
    }
}

impl From<String> for MapEntry {
    fn from(name: String) -> Self {
        MapEntry {
            unique_name: name,
            from_current_scope: true,
        }
    }
}

fn copy_variable_map(variable_map: &HashMap<String, MapEntry>) -> HashMap<String, MapEntry> {
    let mut new_map = HashMap::new();
    for (key, value) in variable_map.iter() {
        new_map.insert(key.clone(), value.clone());
    }
    new_map
}

fn resolve_expression(
    expr: &Expression,
    variable_map: &mut HashMap<String, MapEntry>,
) -> Result<Expression, CompilerError> {
    match expr {
        Expression::Var(name) => {
            let map_entry = variable_map
                .get(name)
                .ok_or(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::VariableNotDeclared(name.clone()),
                ))?;
            Ok(Expression::Var(map_entry.unique_name.clone()))
        }
        Expression::Unary(op, expr) => {
            let expr = resolve_expression(expr, variable_map)?;
            Ok(Expression::Unary(op.clone(), Box::new(expr)))
        }
        Expression::BinOp(op, expr1, expr2) => {
            let expr1 = resolve_expression(expr1, variable_map)?;
            let expr2 = resolve_expression(expr2, variable_map)?;
            Ok(Expression::BinOp(
                op.clone(),
                Box::new(expr1),
                Box::new(expr2),
            ))
        }
        Expression::Assignment(expr1, expr2) => match expr1.as_ref() {
            Expression::Var(_name) => {
                let expr1 = resolve_expression(expr1, variable_map)?;
                let expr2 = resolve_expression(expr2, variable_map)?;
                Ok(Expression::Assignment(Box::new(expr1), Box::new(expr2)))
            }
            _ => Err(CompilerError::SemanticAnalysis(
                SemanticAnalysisError::InvalidLValue,
            )),
        },
        Expression::Constant(_) => Ok(expr.clone()),
        Expression::Conditional(cond, then, els) => {
            let cond = resolve_expression(cond, variable_map)?;
            let then = resolve_expression(then, variable_map)?;
            let els = resolve_expression(els, variable_map)?;
            Ok(Expression::Conditional(
                Box::new(cond),
                Box::new(then),
                Box::new(els),
            ))
        }
    }
}

fn resolve_declaration(
    decl: Declaration,
    variable_map: &mut HashMap<String, MapEntry>,
) -> Result<Declaration, CompilerError> {
    if variable_map.contains_key(&decl.name) && variable_map[&decl.name].from_current_scope {
        return Err(CompilerError::SemanticAnalysis(
            crate::error::SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
        ));
    }
    let unique_name = format!("{}__{}", decl.name, variable_map.len());
    variable_map.insert(decl.name, unique_name.clone().into());
    let init = match decl.value {
        Some(expr) => {
            let expression = resolve_expression(&expr, variable_map)?;
            Some(expression)
        }
        None => None,
    };
    Ok(Declaration {
        name: unique_name,
        value: init,
    })
}

fn resolve_block(
    blocks: &[BlockItem],
    variable_map: &mut HashMap<String, MapEntry>,
) -> Result<Vec<BlockItem>, CompilerError> {
    let mut new_block = Vec::new();
    for item in blocks {
        match item {
            BlockItem::Declaration(decl) => {
                let decl = resolve_declaration(decl.clone(),variable_map)?;
                new_block.push(BlockItem::Declaration(decl));
            }
            BlockItem::Statement(stmt) => {
                let stmt = resolve_statement(stmt, variable_map)?;
                new_block.push(BlockItem::Statement(stmt));
            }
        }
    }
    Ok(new_block)
}

fn resolve_for_init(
    init: &ForInit,
    variable_map: &mut HashMap<String, MapEntry>,
) -> Result<ForInit, CompilerError> {
    match init {
        ForInit::InitDeclaration(decl) => {
            let decl = resolve_declaration(decl.clone(), variable_map)?;
            Ok(ForInit::InitDeclaration(decl))
        }
        ForInit::InitExpression(Some(expr)) => {
            let expr = resolve_expression(expr, variable_map)?;
            Ok(ForInit::InitExpression(Some(expr)))
        }
        ForInit::InitExpression(None) => {
            Ok(ForInit::InitExpression(None))
        }
    }
}

fn resolve_statement(
    stmt: &Statement,
    variable_map: &mut HashMap<String, MapEntry>,
) -> Result<Statement, CompilerError> {
    match stmt {
        Statement::Return(expr) => {
            let expr = resolve_expression(expr, variable_map)?;
            Ok(Statement::Return(expr))
        }
        Statement::Expression(expr) => {
            let expr = resolve_expression(expr, variable_map)?;
            Ok(Statement::Expression(expr))
        }
        Statement::If(expr, then, els) => {
            let expr = resolve_expression(expr, variable_map)?;
            let then = resolve_statement(then.as_ref(), variable_map)?;
            let els = match els {
                Some(els) => Some(Box::new(resolve_statement(els, variable_map)?)),
                None => None,
            };
            Ok(Statement::If(expr, Box::new(then), els))
        }
        Statement::Compound(blocks) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let blocks = resolve_block(blocks, &mut new_variable_map)?;
            Ok(Statement::Compound(blocks))
        },
        Statement::Null => Ok(Statement::Null),
        Statement::For(init, cond , post , body, loop_id) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let for_init = resolve_for_init(init, &mut new_variable_map)?;
            let cond = cond.clone().map(|expr| resolve_expression(&expr, &mut new_variable_map)).transpose()?;
            let post = post.clone().map(|expr| resolve_expression(&expr, &mut new_variable_map)).transpose()?;
            let body = resolve_statement(body, &mut new_variable_map)?;
            Ok(Statement::For(for_init, cond, post, Box::new(body), loop_id.clone()))
        }
        Statement::DoWhile(body, cond , loop_id) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            let body = resolve_statement(body, &mut new_variable_map)?;
            let cond = resolve_expression(cond, variable_map)?;
            Ok(Statement::DoWhile(Box::new(body), cond, loop_id.clone()))
        }
        Statement::While(cond, body, loop_id) => {
            let cond = resolve_expression(cond, variable_map)?;
            let mut new_variable_map = copy_variable_map(variable_map);
            let body = resolve_statement(body, &mut new_variable_map)?;
            Ok(Statement::While(cond, Box::new(body), loop_id.clone()))
        }
        Statement::Break(loop_id) => Ok(Statement::Break(loop_id.clone())),
        Statement::Continue(loop_id) => Ok(Statement::Continue(loop_id.clone())),
        //d => Ok(d.clone()), //TODO: Implement the rest of the statements
    }
}

fn resolve_function(function: Function,) -> Result<Function, CompilerError> {
    let mut variable_map = HashMap::new();
    let mut new_body = Vec::new();
    for item in function.body {
        match item {
            BlockItem::Declaration(decl) => {
                let decl = resolve_declaration(decl, &mut variable_map)?;
                new_body.push(BlockItem::Declaration(decl));
            }
            BlockItem::Statement(stmt) => {
                let stmt = resolve_statement(&stmt, &mut variable_map)?;
                new_body.push(BlockItem::Statement(stmt));
            }
        }
    }
    Ok(Function {
        name: function.name,
        body: new_body,
    })
}

pub fn semantic_validation(program: Program) -> Result<Program, CompilerError> {
    let function = resolve_function(program.function)?;
    Ok(Program {
        function,
    })
}