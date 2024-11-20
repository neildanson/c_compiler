use crate::ast::*;
use crate::parse::Expression;

type LoopIdentifier = Identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<E: Clone> {
    Return(E),
    Expression(E),
    If(E, Box<Statement<E>>, Option<Box<Statement<E>>>),
    Compound(Vec<BlockItem<Statement<E>, E>>),
    Null,
    Break(LoopIdentifier),
    Continue(LoopIdentifier),
    While(E, Box<Statement<E>>, LoopIdentifier),
    DoWhile(Box<Statement<E>>, E, LoopIdentifier),
    For(
        ForInit<E>,
        Option<E>,
        Option<E>,
        Box<Statement<E>>,
        LoopIdentifier,
    ),
}

impl From<BlockItem<crate::parse::Statement<Expression>, Expression>>
    for BlockItem<Statement<Expression>, Expression>
{
    fn from(item: BlockItem<crate::parse::Statement<Expression>, Expression>) -> Self {
        match item {
            BlockItem::Declaration(decl) => BlockItem::Declaration(decl.into()),
            BlockItem::Statement(stmt) => BlockItem::Statement(stmt.into()),
        }
    }
}

impl From<FunctionDeclaration<crate::parse::Statement<Expression>, Expression>>
    for FunctionDeclaration<Statement<Expression>, Expression>
{
    fn from(
        function: FunctionDeclaration<crate::parse::Statement<Expression>, Expression>,
    ) -> Self {
        let body = match function.body {
            Some(body) => {
                let body = body.into_iter().map(|item| (item).into()).collect();
                Some(body)
            }
            None => None,
        };
        FunctionDeclaration::new(
            function.name,
            function.parameters,
            body,
            function.fun_type,
            function.storage_class,
        )
    }
}

impl From<Declaration<crate::parse::Statement<Expression>, Expression>>
    for Declaration<Statement<Expression>, Expression>
{
    fn from(decl: Declaration<crate::parse::Statement<Expression>, Expression>) -> Self {
        match decl {
            Declaration::Variable(variable) => Declaration::Variable(variable.clone()),
            Declaration::Function(function) => {
                let function = function.into();
                Declaration::Function(function)
            }
        }
    }
}

impl From<crate::parse::Statement<Expression>> for Statement<Expression> {
    fn from(stmt: crate::parse::Statement<Expression>) -> Self {
        match stmt {
            crate::parse::Statement::Return(e) => Statement::Return(e.clone()),
            crate::parse::Statement::Expression(e) => Statement::Expression(e.clone()),
            crate::parse::Statement::If(cond, then, els) => {
                let then = Box::new(then.as_ref().clone().into());
                let els = els.map(|els| Box::new(els.as_ref().clone().into()));
                Statement::If(cond.clone(), then, els)
            }
            crate::parse::Statement::Compound(block) => {
                let new_block: Vec<BlockItem<Statement<Expression>, Expression>> = block
                    .into_iter()
                    .map(|item| match item {
                        BlockItem::Declaration(decl) => BlockItem::Declaration(decl.clone().into()),
                        BlockItem::Statement(stmt) => BlockItem::Statement(stmt.into()),
                    })
                    .collect();
                Statement::Compound(new_block)
            }
            crate::parse::Statement::Null => Statement::Null,
            crate::parse::Statement::Break
            | crate::parse::Statement::Continue
            | crate::parse::Statement::While(_, _)
            | crate::parse::Statement::DoWhile(_, _)
            | crate::parse::Statement::For(_, _, _, _) => panic!("Invalid statement conversion"),
        }
    }
}
