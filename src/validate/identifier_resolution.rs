use crate::ast::*;
use crate::error::*;
use crate::parse::Expression;
use crate::parse::Statement;
use global_counter::primitive::exact::CounterI32;
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct MapEntry {
    unique_name: Identifier,
    from_current_scope: bool,
    has_external_linkage: bool,
}

impl MapEntry {
    pub(crate) fn new(
        unique_name: Identifier,
        from_current_scope: bool,
        has_external_linkage: bool,
    ) -> Self {
        MapEntry {
            unique_name,
            from_current_scope,
            has_external_linkage,
        }
    }
}

impl Clone for MapEntry {
    fn clone(&self) -> Self {
        MapEntry {
            unique_name: self.unique_name.clone(),
            from_current_scope: false, //Is this a bit naughty changing on clone?
            has_external_linkage: self.has_external_linkage,
        }
    }
}

impl From<String> for MapEntry {
    fn from(name: String) -> Self {
        MapEntry {
            unique_name: name,
            from_current_scope: true,
            has_external_linkage: false,
        }
    }
}

static COUNTER: CounterI32 = CounterI32::new(0);

#[derive(Default)]
pub(crate) struct IdentifierResolution {
    identifier_map: HashMap<Identifier, MapEntry>,
}

impl Clone for IdentifierResolution {
    fn clone(&self) -> Self {
        let mut new_map = HashMap::new();
        for (key, value) in self.identifier_map.iter() {
            new_map.insert(key.clone(), value.clone());
        }
        IdentifierResolution {
            identifier_map: new_map,
        }
    }
}

impl IdentifierResolution {
    fn make_unique_name(&mut self, name: String) -> String {
        let count = COUNTER.inc();
        let unique_name = format!("{}__{}", name, count);

        self.identifier_map.insert(name, unique_name.clone().into());

        unique_name
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Result<Expression, CompilerError> {
        match expr {
            Expression::Var(name) => {
                let map_entry =
                    self.identifier_map
                        .get(name)
                        .ok_or(CompilerError::SemanticAnalysis(
                            SemanticAnalysisError::VariableNotDeclared(name.clone()),
                        ))?;
                Ok(Expression::Var(map_entry.unique_name.clone()))
            }
            Expression::Unary(op, expr) => {
                let expr = self.resolve_expression(expr)?;
                Ok(Expression::Unary(op.clone(), Box::new(expr)))
            }
            Expression::BinOp(op, expr1, expr2) => {
                let expr1 = self.resolve_expression(expr1)?;
                let expr2 = self.resolve_expression(expr2)?;
                Ok(Expression::BinOp(
                    op.clone(),
                    Box::new(expr1),
                    Box::new(expr2),
                ))
            }
            Expression::Assignment(expr1, expr2) => match expr1.as_ref() {
                Expression::Var(_) => {
                    let expr1 = self.resolve_expression(expr1)?;
                    let expr2 = self.resolve_expression(expr2)?;
                    Ok(Expression::Assignment(Box::new(expr1), Box::new(expr2)))
                }
                _ => Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::InvalidLValue,
                )),
            },
            Expression::Constant(_) => Ok(expr.clone()),
            Expression::Conditional(cond, then, els) => {
                let cond = self.resolve_expression(cond)?;
                let then = self.resolve_expression(then)?;
                let els = self.resolve_expression(els)?;
                Ok(Expression::Conditional(
                    Box::new(cond),
                    Box::new(then),
                    Box::new(els),
                ))
            }
            Expression::FunctionCall(name, args) => {
                let ident = self.identifier_map.get(name);
                match ident {
                    Some(ident) => {
                        let unique_name = ident.unique_name.clone();
                        let args = args
                            .iter()
                            .map(|arg| self.resolve_expression(arg))
                            .collect::<Result<Vec<Expression>, CompilerError>>()?;
                        Ok(Expression::FunctionCall(unique_name, args))
                    }
                    None => Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionNotDeclared(name.clone()),
                    )),
                }
            }
            Expression::Cast(ty, expr) => {
                let expr = self.resolve_expression(expr)?;
                Ok(Expression::Cast(ty.clone(), Box::new(expr)))
            }
        }
    }

    pub fn resolve_file_scope_variable_declaration(
        &mut self,
        decl: VariableDeclaration<Expression>,
    ) -> Result<VariableDeclaration<Expression>, CompilerError> {
        self.identifier_map.insert(
            decl.name.clone(),
            MapEntry::new(decl.name.clone(), true, true),
        );
        Ok(decl)
    }

    fn resolve_local_variable_declaration(
        &mut self,
        decl: VariableDeclaration<Expression>,
    ) -> Result<VariableDeclaration<Expression>, CompilerError> {
        if let Some(entry) = self.identifier_map.get(&decl.name) {
            if entry.from_current_scope
                && !(entry.has_external_linkage && decl.storage_class == Some(StorageClass::Extern))
            {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::VariableAlreadyDeclared(decl.name),
                ));
            }
        }

        if decl.storage_class == Some(StorageClass::Extern) {
            self.identifier_map.insert(
                decl.name.clone(),
                MapEntry::new(decl.name.clone(), true, true),
            );
            return Ok(decl);
        }

        let unique_name = self.make_unique_name(decl.name);

        let init = match decl.init {
            Some(expr) => {
                let expression = self.resolve_expression(&expr)?;
                Some(expression)
            }
            None => None,
        };
        Ok(VariableDeclaration {
            name: unique_name,
            init,
            var_type: decl.var_type,
            storage_class: decl.storage_class,
        })
    }

    //This is very similar to resolve_variable_declaration
    //Consider refactoring
    fn resolve_param(
        &mut self,
        ty: &Type,
        param: Identifier,
    ) -> Result<(Type, Identifier), CompilerError> {
        if self.identifier_map.contains_key(&param)
            && self.identifier_map[&param].from_current_scope
        {
            return Err(CompilerError::SemanticAnalysis(
                crate::error::SemanticAnalysisError::VariableAlreadyDeclared(param.clone()),
            ));
        }
        let unique_name = self.make_unique_name(param);
        Ok((ty.clone(), unique_name))
    }

    pub fn resolve_function_declaration(
        &mut self,
        decl: FunctionDeclaration<Statement<Expression>, Expression>,
        nested: bool,
    ) -> Result<FunctionDeclaration<Statement<Expression>, Expression>, CompilerError> {
        if let Some(entry) = self.identifier_map.get(&decl.name) {
            if entry.from_current_scope && !entry.has_external_linkage {
                {
                    return Err(CompilerError::SemanticAnalysis(
                        SemanticAnalysisError::FunctionAlreadyDeclared(decl.name),
                    ));
                }
            }
        }

        let unique_name = decl.name.clone();
        let map_entry = MapEntry::new(unique_name.clone(), true, true);
        self.identifier_map.insert(decl.name, map_entry);

        let mut inner_scope = self.clone();

        let parameters = decl
            .parameters
            .iter()
            .map(|(ty, param)| inner_scope.resolve_param(ty, param.clone()))
            .collect::<Result<Vec<(Type, Identifier)>, CompilerError>>()?;

        let body = if let Some(body) = decl.body {
            if nested {
                return Err(CompilerError::SemanticAnalysis(
                    SemanticAnalysisError::NestedFunction,
                ));
            }
            let body = inner_scope.resolve_block(&body)?;
            Some(body)
        } else {
            None
        };

        Ok(FunctionDeclaration::new(
            unique_name,
            parameters,
            body,
            decl.fun_type,
            decl.storage_class,
        ))
    }

    fn resolve_block(
        &mut self,
        blocks: &[BlockItem<Statement<Expression>, Expression>],
    ) -> Result<Vec<BlockItem<Statement<Expression>, Expression>>, CompilerError> {
        let mut new_block = Vec::new();
        for item in blocks {
            match item {
                BlockItem::Declaration(Declaration::Variable(decl)) => {
                    let decl = self.resolve_local_variable_declaration(decl.clone())?;
                    new_block.push(BlockItem::Declaration(Declaration::Variable(decl)));
                }
                BlockItem::Declaration(Declaration::Function(decl)) => {
                    let decl = self.resolve_function_declaration(decl.clone(), true)?;
                    new_block.push(BlockItem::Declaration(Declaration::Function(decl)));
                }
                BlockItem::Statement(stmt) => {
                    let stmt = self.resolve_statement(stmt)?;
                    new_block.push(BlockItem::Statement(stmt));
                } //_ => {
                  //    return Err(CompilerError::SemanticAnalysis(
                  //        SemanticAnalysisError::InvalidBlockItem,
                  //    ))
                  //}
            }
        }
        Ok(new_block)
    }

    fn resolve_for_init(
        &mut self,
        init: &ForInit<Expression>,
    ) -> Result<ForInit<Expression>, CompilerError> {
        match init {
            ForInit::InitDeclaration(decl) => {
                let decl = self.resolve_local_variable_declaration(decl.clone())?;
                Ok(ForInit::InitDeclaration(decl))
            }
            ForInit::InitExpression(Some(expr)) => {
                let expr = self.resolve_expression(expr)?;
                Ok(ForInit::InitExpression(Some(expr)))
            }
            ForInit::InitExpression(None) => Ok(ForInit::InitExpression(None)),
        }
    }

    fn resolve_statement(
        &mut self,
        stmt: &Statement<Expression>,
    ) -> Result<Statement<Expression>, CompilerError> {
        match stmt {
            Statement::Return(expr) => {
                let expr = self.resolve_expression(expr)?;
                Ok(Statement::Return(expr))
            }
            Statement::Expression(expr) => {
                let expr = self.resolve_expression(expr)?;
                Ok(Statement::Expression(expr))
            }
            Statement::If(expr, then, els) => {
                let expr = self.resolve_expression(expr)?;
                let then = self.resolve_statement(then.as_ref())?;
                let els = match els {
                    Some(els) => Some(Box::new(self.resolve_statement(els)?)),
                    None => None,
                };
                Ok(Statement::If(expr, Box::new(then), els))
            }
            Statement::Compound(blocks) => {
                let mut inner_scope = self.clone();
                let blocks = inner_scope.resolve_block(blocks)?;
                Ok(Statement::Compound(blocks))
            }
            Statement::Null => Ok(Statement::Null),
            Statement::For(init, cond, post, body) => {
                let mut inner_scope = self.clone();
                let for_init = inner_scope.resolve_for_init(init)?;
                let cond = cond
                    .clone()
                    .map(|expr| inner_scope.resolve_expression(&expr))
                    .transpose()?;
                let post = post
                    .clone()
                    .map(|expr| inner_scope.resolve_expression(&expr))
                    .transpose()?;
                //let mut inner_scope = inner_scope.clone();
                let body = inner_scope.resolve_statement(body)?;
                Ok(Statement::For(for_init, cond, post, Box::new(body)))
            }
            Statement::DoWhile(body, cond) => {
                let mut inner_scope = self.clone();
                let body = inner_scope.resolve_statement(body)?;
                let cond = self.resolve_expression(cond)?;
                Ok(Statement::DoWhile(Box::new(body), cond))
            }
            Statement::While(cond, body) => {
                let cond = self.resolve_expression(cond)?;
                let mut inner_scope = self.clone();
                let body = inner_scope.resolve_statement(body)?;
                Ok(Statement::While(cond, Box::new(body)))
            }
            Statement::Break => Ok(Statement::Break),
            Statement::Continue => Ok(Statement::Continue),
        }
    }
}
