use super::type_checker;
use super::{identifier_resolution::*, loop_labelling::LoopLabelling};
use crate::error::*;
use crate::parse::ast::*;

#[derive(Default)]
pub struct SemanticAnalysis;

impl SemanticAnalysis {
    fn semantic_validation_function(
        function: FunctionDefinition,
        identifier_resolution: &mut IdentifierResolution,
        loop_labelling: &mut LoopLabelling,
        type_checker: &mut type_checker::TypeChecker,
    ) -> Result<FunctionDefinition, CompilerError> {
        let function = identifier_resolution.resolve_function_declaration(function, false)?;
        let function = loop_labelling.label_function(function)?;
        let function = loop_labelling.verify_function_labels(function)?;
        type_checker.type_check_function_declaration(&function)?;
        Ok(function)
    }

    pub fn semantic_validation(program: Program) -> Result<Program, CompilerError> {
        let mut identifier_resolution = IdentifierResolution::default();
        let mut loop_labelling = LoopLabelling::default();
        let mut type_checker = type_checker::TypeChecker::default();

        let mut functions = Vec::new();
        for function in program.functions {
            let function = Self::semantic_validation_function(
                function,
                &mut identifier_resolution,
                &mut loop_labelling,
                &mut type_checker,
            )?;
            functions.push(function);
        }

        Ok(Program { functions })
    }
}
