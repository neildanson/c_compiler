mod identifier_resolution;
pub mod loop_labelling;
pub mod semantic_analysis;
pub mod type_checker;

use loop_labelling::Statement;
use std::collections::HashMap;

pub use type_checker::*;

use crate::parse::Program;

#[derive(Debug)]
pub struct ValidateResult {
    pub program: Program<Statement<Expression>, Expression>,
    pub symbols: HashMap<String, Symbol>,
}
