use std::fmt::{Display, Formatter};

use crate::ast::{BinaryOperator, Constant, Identifier, Type, UnaryOperator};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Var(Identifier, Type),
    Unary(UnaryOperator, Box<Expression>, Type),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>, Type),
    Assignment(Box<Expression>, Box<Expression>, Type),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>, Type),
    FunctionCall(Identifier, Vec<Expression>, Type),
    Cast(Type, Box<Expression>),
    Constant(Constant),
}

impl Expression {
    pub fn with_type(expr: &crate::parse::Expression, ty: Type) -> Expression {
        match expr {
            crate::parse::Expression::Var(name) => Expression::Var(name.clone(), ty),
            crate::parse::Expression::Unary(op, expr) => {
                Expression::Unary(op.clone(), Box::new(Self::with_type(expr, ty.clone())), ty)
            }
            crate::parse::Expression::BinOp(op, left, right) => Expression::BinOp(
                op.clone(),
                Box::new(Self::with_type(left, ty.clone())),
                Box::new(Self::with_type(right, ty.clone())),
                ty,
            ),
            crate::parse::Expression::Assignment(left, right) => Expression::Assignment(
                Box::new(Self::with_type(left, ty.clone())),
                Box::new(Self::with_type(right, ty.clone())),
                ty,
            ),
            crate::parse::Expression::Conditional(condition, then_expression, else_expression) => {
                Expression::Conditional(
                    Box::new(Self::with_type(condition, ty.clone())),
                    Box::new(Self::with_type(then_expression, ty.clone())),
                    Box::new(Self::with_type(else_expression, ty.clone())),
                    ty,
                )
            }
            crate::parse::Expression::FunctionCall(name, arguments) => Expression::FunctionCall(
                name.clone(),
                arguments
                    .iter()
                    .map(|arg| Self::with_type(arg, ty.clone()))
                    .collect(),
                ty,
            ),
            crate::parse::Expression::Cast(ty, expr) => {
                Expression::Cast(ty.clone(), Box::new(Self::with_type(expr, ty.clone())))
            }
            crate::parse::Expression::Constant(c) => Expression::Constant(*c),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Expression::Var(_, t) => t.clone(),
            Expression::Unary(_, _, t) => t.clone(),
            Expression::BinOp(_, _, _, t) => t.clone(),
            Expression::Assignment(_, _, t) => t.clone(),
            Expression::Conditional(_, _, _, t) => t.clone(),
            Expression::FunctionCall(_, _, t) => t.clone(),
            Expression::Cast(t, _) => t.clone(),
            Expression::Constant(c) => c.get_type(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Expression::Var(ident, ty) => write!(f, "{} {} ", ty, ident),
            Expression::BinOp(op, lhs, rhs, _) => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::Cast(t, expr) => write!(f, "({} as {})", expr, t),
            Expression::Unary(op, expr, _) => write!(f, "({} {})", op, expr),
            Expression::Assignment(lhs, rhs, _) => write!(f, "({} = {})", lhs, rhs),
            Expression::Conditional(cond, then_expr, else_expr, _) => {
                write!(f, "if {} then {} else {}", cond, then_expr, else_expr)
            }
            Expression::FunctionCall(name, _, _) => {
                write!(f, "{}()", name)
            }
            Expression::Constant(c) => write!(f, "{}", c),

            //_ => write!(f, "{:?}", self) //TODO
        }

    }
}