use crate::error::*;
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

#[derive(Default)]
pub(crate) struct LoopLabelling {
    label_count: u32,
}

impl LoopLabelling {
    fn make_label(&mut self) -> String {
        let name = format!("__{}", self.label_count);
        self.label_count += 1;
        name
    }

    fn label_block(
        &mut self,
        blocks: &[BlockItem<Statement<Expression>, Expression>],
        current_label: Option<String>,
    ) -> Result<Vec<BlockItem<LLStatement<Expression>, Expression>>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Statement(stmt) => {
                    let stmt = self.label_statement(stmt.clone(), current_label.clone())?;
                    new_block.push(BlockItem::Statement(stmt));
                }
                BlockItem::Declaration(decl) => {
                    new_block.push(BlockItem::Declaration(decl.clone().into()));
                }
            }
        }
        Ok(new_block)
    }

    fn label_statement(
        &mut self,
        stmt: Statement<Expression>,
        current_label: Option<String>,
    ) -> Result<LLStatement<Expression>, CompilerError> {
        match stmt {
            Statement::Break => {
                if let Some(label) = current_label {
                    Ok(LLStatement::Break(label))
                } else {
                    Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidBreak,
                    ))
                }
            }
            Statement::Continue => {
                if let Some(label) = current_label {
                    Ok(LLStatement::Continue(label))
                } else {
                    Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidContinue,
                    ))
                }
            }
            Statement::While(condition, body) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = LLStatement::While(condition.clone(), Box::new(body), label);
                Ok(new_stmt)
            }
            Statement::DoWhile(body, condition) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = LLStatement::DoWhile(Box::new(body), condition.clone(), label);
                Ok(new_stmt)
            }
            Statement::For(init, condition, post, body) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = LLStatement::For(
                    init.clone(),
                    condition.clone(),
                    post.clone(),
                    Box::new(body),
                    label,
                );
                Ok(new_stmt)
            }
            Statement::If(cond, then, els) => {
                let then = self.label_statement(*then, current_label.clone())?;
                let els = match els {
                    Some(els) => Some(Box::new(self.label_statement(*els, current_label.clone())?)),
                    None => None,
                };
                Ok(LLStatement::If(cond.clone(), Box::new(then), els))
            }
            Statement::Compound(block) => {
                let new_block = self.label_block(&block, current_label)?;
                Ok(LLStatement::Compound(new_block))
            }
            _ => Ok(stmt.into()),
        }
    }

    pub fn label_function(
        &mut self,
        function: FunctionDeclaration<Statement<Expression>, Expression>,
    ) -> Result<FunctionDeclaration<LLStatement<Expression>, Expression>, CompilerError> {
        match function.body {
            Some(body) => {
                let new_body = self.label_block(&body, None)?;
                Ok(FunctionDeclaration::new(
                    function.name,
                    function.parameters,
                    Some(new_body),
                    function.fun_type,
                    function.storage_class,
                ))
            }
            None => Ok(function.into()),
        }
    }

    /*
    fn verify_statement_labels(stmt: Statement<Expression>) -> Result<Statement<Expression>, CompilerError> {
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

    pub fn verify_function_labels(
        &self,
        function: FunctionDeclaration<Statement<Expression>, Expression>,
    ) -> Result<FunctionDeclaration<Statement<Expression>, Expression>, CompilerError> {
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
            Ok(FunctionDeclaration::new(function.name,function.parameters, Some(new_body),function.fun_type, function.storage_class))
        } else {
            Ok(FunctionDeclaration::new(function.name,function.parameters, None, function.fun_type, function.storage_class))
        }
    } */
}
