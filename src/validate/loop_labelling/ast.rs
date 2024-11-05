use crate::parse::ast::*;

type LoopIdentifier = Identifier;

#[derive(Clone, Debug, PartialEq)]
pub enum LLStatement<E: Clone> {
    Return(E),
    Expression(E),
    If(E, Box<LLStatement<E>>, Option<Box<LLStatement<E>>>),
    Compound(Vec<BlockItem<LLStatement<E>, E>>),
    Null,
    Break(LoopIdentifier),
    Continue(LoopIdentifier),
    While(E, Box<LLStatement<E>>, LoopIdentifier),
    DoWhile(Box<LLStatement<E>>, E, LoopIdentifier),
    For(
        ForInit<E>,
        Option<E>,
        Option<E>,
        Box<LLStatement<E>>,
        LoopIdentifier,
    ),
}

impl From<BlockItem<Statement<Expression>, Expression>>
    for BlockItem<LLStatement<Expression>, Expression>
{
    fn from(item: BlockItem<Statement<Expression>, Expression>) -> Self {
        match item {
            BlockItem::Declaration(decl) => BlockItem::Declaration(decl.into()),
            BlockItem::Statement(stmt) => BlockItem::Statement(stmt.into()),
        }
    }
}

impl From<FunctionDeclaration<Statement<Expression>, Expression>>
    for FunctionDeclaration<LLStatement<Expression>, Expression>
{
    fn from(function: FunctionDeclaration<Statement<Expression>, Expression>) -> Self {
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

impl From<Declaration<Statement<Expression>, Expression>>
    for Declaration<LLStatement<Expression>, Expression>
{
    fn from(decl: Declaration<Statement<Expression>, Expression>) -> Self {
        match decl {
            Declaration::Variable(variable) => Declaration::Variable(variable.clone()),
            Declaration::Function(function) => {
                let function = function.into();
                Declaration::Function(function)
            }
        }
    }
}

impl From<Statement<Expression>> for LLStatement<Expression> {
    fn from(stmt: Statement<Expression>) -> Self {
        match stmt {
            Statement::Return(e) => LLStatement::Return(e.clone()),
            Statement::Expression(e) => LLStatement::Expression(e.clone()),
            Statement::If(cond, then, els) => {
                let then = Box::new(then.as_ref().clone().into());
                let els = els.map(|els| Box::new(els.as_ref().clone().into()));
                LLStatement::If(cond.clone(), then, els)
            }
            Statement::Compound(block) => {
                let new_block: Vec<BlockItem<LLStatement<Expression>, Expression>> = block
                    .into_iter()
                    .map(|item| match item {
                        BlockItem::Declaration(decl) => BlockItem::Declaration(decl.clone().into()),
                        BlockItem::Statement(stmt) => BlockItem::Statement(stmt.into()),
                    })
                    .collect();
                LLStatement::Compound(new_block)
            }
            Statement::Null => LLStatement::Null,
            Statement::Break
            | Statement::Continue
            | Statement::While(_, _)
            | Statement::DoWhile(_, _)
            | Statement::For(_, _, _, _) => panic!("Invalid statement conversion"),
        }
    }
}
