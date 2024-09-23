use crate::error::*;
use super::ast::*;   
use std::collections::HashMap;

fn resolve_expression(
    expr: &Expression,
    variable_map: &mut HashMap<String, String>,
) -> Result<Expression, CompilerError> {
    match expr {
        Expression::Var(name) => {
            let unique_name = variable_map
                .get(name)
                .ok_or(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::VariableNotDeclared(name.clone()),
                ))?;
            Ok(Expression::Var(unique_name.clone()))
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

fn resolve_declatation(
    decl: Declaration,
    variable_map: &mut HashMap<String, String>,
) -> Result<Declaration, CompilerError> {
    if variable_map.contains_key(&decl.name) {
        return Err(CompilerError::SemanticAnalysis(
            crate::error::SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
        ));
    }
    let unique_name = format!("{}__{}", decl.name, variable_map.len());
    variable_map.insert(decl.name, unique_name.clone());
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

fn semantic_validation_statement(
    stmt: &Statement,
    variable_map: &mut HashMap<String, String>,
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
            let then = semantic_validation_statement(then.as_ref(), variable_map)?;
            let els = match els {
                Some(els) => Some(Box::new(semantic_validation_statement(els, variable_map)?)),
                None => None,
            };
            Ok(Statement::If(expr, Box::new(then), els))
        }
        Statement::Null => Ok(Statement::Null),
    }
}

pub fn semantic_validation(program: Program) -> Result<Program, CompilerError> {
    let mut variable_map = HashMap::new();
    let mut new_body = Vec::new();
    for item in program.function.body {
        match item {
            BlockItem::Declaration(decl) => {
                let decl = resolve_declatation(decl, &mut variable_map)?;
                new_body.push(BlockItem::Declaration(decl));
            }
            BlockItem::Statement(stmt) => {
                let stmt = semantic_validation_statement(&stmt, &mut variable_map)?;
                new_body.push(BlockItem::Statement(stmt));
            }
        }
    }
    Ok(Program {
        function: Function {
            name: program.function.name,
            body: new_body,
        },
    })
}