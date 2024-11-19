mod identifier_resolution;
pub mod loop_labelling;
pub mod semantic_analysis;
pub mod type_checker;

use loop_labelling::LLStatement;
use std::collections::HashMap;

pub use type_checker::*;

use crate::parse::Program;

#[derive(Debug)]
pub struct ValidateResult {
    pub program: Program<LLStatement<Expression>, Expression>,
    pub symbols: HashMap<String, Symbol>,
}
