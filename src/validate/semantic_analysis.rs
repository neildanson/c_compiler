use super::{identifier_resolution::*, loop_labelling::LoopLabelling};
use crate::error::*;
use crate::parse::ast::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct SemanticAnalysis;

impl SemanticAnalysis {
    fn semantic_validation_function(
        function: FunctionDefinition,
        identifier_resolution: &mut IdentifierResolution,
        loop_labelling: &mut LoopLabelling,
        identifier_map: &mut HashMap<Identifier, MapEntry>,
    ) -> Result<FunctionDefinition, CompilerError> {
        let function =
            identifier_resolution.resolve_function_declaration(function, identifier_map, false)?;
        let function = loop_labelling.label_function(function)?;
        let function = loop_labelling.verify_function_labels(function)?;

        Ok(function)
    }

    pub fn semantic_validation(program: Program) -> Result<Program, CompilerError> {
        let mut identifier_resolution = IdentifierResolution::default();
        let mut loop_labelling = LoopLabelling::default();

        let mut functions = Vec::new();
        let mut identifier_map = HashMap::new();
        for function in program.functions {
            let function = Self::semantic_validation_function(
                function,
                &mut identifier_resolution,
                &mut loop_labelling,
                &mut identifier_map,
            )?;
            functions.push(function);
        }

        Ok(Program { functions })
    }
}
