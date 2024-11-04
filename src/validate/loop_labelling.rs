use crate::error::*;
use crate::parse::ast::*;

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
    ) -> Result<Vec<BlockItem<Statement<Expression>, Expression>>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Declaration(decl) => {
                    new_block.push(BlockItem::Declaration(decl.clone()));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = self.label_statement(stmt.clone(), current_label.clone())?;
                    new_block.push(BlockItem::Statement(stmt));
                }
            }
        }
        Ok(new_block)
    }

    fn label_statement(
        &mut self,
        stmt: Statement<Expression>,
        current_label: Option<String>,
    ) -> Result<Statement<Expression>, CompilerError> {
        match stmt {
            Statement::Break(_) => {
                if current_label.is_none() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidBreak,
                    ));
                }
                Ok(Statement::Break(current_label))
            }
            Statement::Continue(_) => {
                if current_label.is_none() {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::InvalidContinue,
                    ));
                }
                Ok(Statement::Continue(current_label))
            }
            Statement::While(condition, body, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::While(condition, Box::new(body), Some(label));
                Ok(new_stmt)
            }
            Statement::DoWhile(body, condition, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::DoWhile(Box::new(body), condition, Some(label));
                Ok(new_stmt)
            }
            Statement::For(init, condition, post, body, _) => {
                let label = self.make_label();
                let body = self.label_statement(*body, Some(label.clone()))?;
                let new_stmt = Statement::For(init, condition, post, Box::new(body), Some(label));
                Ok(new_stmt)
            }
            Statement::If(cond, then, els) => {
                let then = self.label_statement(*then, current_label.clone())?;
                let els = match els {
                    Some(els) => Some(Box::new(self.label_statement(*els, current_label.clone())?)),
                    None => None,
                };
                Ok(Statement::If(cond, Box::new(then), els))
            }
            Statement::Compound(block) => {
                let new_block = self.label_block(&block, current_label)?;
                Ok(Statement::Compound(new_block))
            }
            _ => Ok(stmt.clone()),
        }
    }

    pub fn label_function(
        &mut self,
        function: FunctionDeclaration<Statement<Expression>, Expression>,
    ) -> Result<FunctionDeclaration<Statement<Expression>, Expression>, CompilerError> {
        match function.body {
            Some(body) => {
                let new_body = self.label_block(&body, None)?;
                Ok(FunctionDeclaration::new(function.name, function.parameters, Some(new_body), function.fun_type, function.storage_class))
            }
            None => Ok(function.clone()),
        }
    }

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
    }
}
