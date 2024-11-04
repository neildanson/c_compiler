use crate::parse::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TCExpression {
    Var(Identifier, Type),
    Unary(UnaryOperator, Box<TCExpression>, Type),
    BinOp(BinaryOperator, Box<TCExpression>, Box<TCExpression>, Type),
    Assignment(Box<TCExpression>, Box<TCExpression>, Type),
    Conditional(
        Box<TCExpression>,
        Box<TCExpression>,
        Box<TCExpression>,
        Type,
    ),
    FunctionCall(Identifier, Vec<TCExpression>, Type),
    Cast(Type, Box<TCExpression>),
    Constant(Constant),
}

impl TCExpression {
    pub fn with_type(expr: &Expression, ty: Type) -> TCExpression {
        match expr {
            Expression::Var(name) => TCExpression::Var(name.clone(), ty),
            Expression::Unary(op, expr) => {
                TCExpression::Unary(op.clone(), Box::new(Self::with_type(expr, ty.clone())), ty)
            }
            Expression::BinOp(op, left, right) => TCExpression::BinOp(
                op.clone(),
                Box::new(Self::with_type(left, ty.clone())),
                Box::new(Self::with_type(right, ty.clone())),
                ty,
            ),
            Expression::Assignment(left, right) => TCExpression::Assignment(
                Box::new(Self::with_type(left, ty.clone())),
                Box::new(Self::with_type(right, ty.clone())),
                ty,
            ),
            Expression::Conditional(condition, then_expression, else_expression) => {
                TCExpression::Conditional(
                    Box::new(Self::with_type(condition, ty.clone())),
                    Box::new(Self::with_type(then_expression, ty.clone())),
                    Box::new(Self::with_type(else_expression, ty.clone())),
                    ty,
                )
            }
            Expression::FunctionCall(name, arguments) => TCExpression::FunctionCall(
                name.clone(),
                arguments
                    .iter()
                    .map(|arg| Self::with_type(arg, ty.clone()))
                    .collect(),
                ty,
            ),
            Expression::Cast(ty, expr) => {
                TCExpression::Cast(ty.clone(), Box::new(Self::with_type(expr, ty.clone())))
            }
            Expression::Constant(c) => TCExpression::Constant(c.clone()),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            TCExpression::Var(_, t) => t.clone(),
            TCExpression::Unary(_, _, t) => t.clone(),
            TCExpression::BinOp(_, _, _, t) => t.clone(),
            TCExpression::Assignment(_, _, t) => t.clone(),
            TCExpression::Conditional(_, _, _, t) => t.clone(),
            TCExpression::FunctionCall(_, _, t) => t.clone(),
            TCExpression::Cast(t, _) => t.clone(),
            TCExpression::Constant(c) => c.get_type(),
        }
    }
}
