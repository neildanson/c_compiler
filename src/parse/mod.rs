pub mod parser;
pub use parser::*;

use crate::ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<E: Clone> {
    Return(E),
    Expression(E),
    If(E, Box<Statement<E>>, Option<Box<Statement<E>>>),
    Compound(Vec<BlockItem<Statement<E>, E>>),
    Null,
    Break,
    Continue,
    While(E, Box<Statement<E>>),
    DoWhile(Box<Statement<E>>, E),
    For(ForInit<E>, Option<E>, Option<E>, Box<Statement<E>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Var(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    Cast(Type, Box<Expression>),
    Constant(Constant),
}
