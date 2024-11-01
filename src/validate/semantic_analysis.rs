use super::type_checker::{self};
use super::ValidateResult;
use super::{identifier_resolution::*, loop_labelling::LoopLabelling};
use crate::error::*;
use crate::parse::ast::*;

#[derive(Default)]
pub struct SemanticAnalysis;

impl SemanticAnalysis {
    fn semantic_validation_function(
        function: FunctionDeclaration,
        identifier_resolution: &mut IdentifierResolution,
        loop_labelling: &mut LoopLabelling,
        type_checker: &mut type_checker::TypeChecker,
    ) -> Result<FunctionDeclaration, CompilerError> {
        let function = identifier_resolution.resolve_function_declaration(function, false)?;
        let function = loop_labelling.label_function(function)?;
        let function = loop_labelling.verify_function_labels(function)?;
        let function = type_checker.type_check_function_declaration(&function, true)?;
        Ok(function)
    }

    pub fn semantic_validation(program: Program) -> Result<ValidateResult, CompilerError> {
        let mut identifier_resolution = IdentifierResolution::default();
        let mut loop_labelling = LoopLabelling::default();
        let mut type_checker = type_checker::TypeChecker::default();

        let mut declarations = Vec::new();
        for declaration in program.declarations {
            match declaration {
                Declaration::Variable(variable) => {
                    let variable =
                        identifier_resolution.resolve_file_scope_variable_declaration(variable)?;
                    type_checker.type_check_file_scope_variable_declaration(&variable)?;
                    declarations.push(Declaration::Variable(variable));
                }
                Declaration::Function(function) => {
                    let function = Self::semantic_validation_function(
                        function,
                        &mut identifier_resolution,
                        &mut loop_labelling,
                        &mut type_checker,
                    )?;
                    declarations.push(Declaration::Function(function));
                }
            }
        }
        let result = ValidateResult {
            program: Program { declarations },
            symbols: type_checker.symbol_table,
        };
        Ok(result)
    }
}
