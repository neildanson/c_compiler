use std::collections::HashMap;
mod identifier_resolution;
pub mod error;
pub mod loop_labelling;
pub mod semantic_analysis;
pub mod type_checker;

use loop_labelling::Statement;

pub use type_checker::{Expression, InitialValue, StaticAttr, Symbol};

use crate::ast::Program;

#[derive(Debug)]
pub struct ValidateResult {
    pub program: Program<Statement<Expression>, Expression>,
    pub symbols: HashMap<String, Symbol>,
}
