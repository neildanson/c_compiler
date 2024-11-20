pub mod ast;
use crate::error::*;
use crate::parse::ast::*;
pub use ast::Statement;

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
        blocks: &[BlockItem<crate::parse::ast::Statement<Expression>, Expression>],
        current_label: Option<String>,
    ) -> Result<Vec<BlockItem<Statement<Expression>, Expression>>, CompilerError> {
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
        stmt: crate::parse::ast::Statement<Expression>,
        current_label: Option<String>,
    ) -> Result<Statement<Expression>, CompilerError> {
        match stmt {
            crate::parse::ast::Statement::Break => {
                if let Some(label) = current_label {
                    Ok(Statement::Break(label))
                } else {
                    Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidBreak,
                    ))
                }
            }
            crate::parse::ast::Statement::Continue => {
                if let Some(label) = current_label {
                    Ok(Statement::Continue(label))
                } else {
                    Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidContinue,
                    ))
                }
            }
            crate::parse::ast::Statement::While(condition, body) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::While(condition.clone(), Box::new(body), label);
                Ok(new_stmt)
            }
            crate::parse::ast::Statement::DoWhile(body, condition) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::DoWhile(Box::new(body), condition.clone(), label);
                Ok(new_stmt)
            }
            crate::parse::ast::Statement::For(init, condition, post, body) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::For(
                    init.clone(),
                    condition.clone(),
                    post.clone(),
                    Box::new(body),
                    label,
                );
                Ok(new_stmt)
            }
            crate::parse::ast::Statement::If(cond, then, els) => {
                let then = self.label_statement(*then, current_label.clone())?;
                let els = match els {
                    Some(els) => Some(Box::new(self.label_statement(*els, current_label.clone())?)),
                    None => None,
                };
                Ok(Statement::If(cond.clone(), Box::new(then), els))
            }
            crate::parse::ast::Statement::Compound(block) => {
                let new_block = self.label_block(&block, current_label)?;
                Ok(Statement::Compound(new_block))
            }
            _ => Ok(stmt.into()),
        }
    }

    pub fn label_function(
        &mut self,
        function: FunctionDeclaration<crate::parse::ast::Statement<Expression>, Expression>,
    ) -> Result<FunctionDeclaration<Statement<Expression>, Expression>, CompilerError> {
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
}
