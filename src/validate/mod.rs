mod identifier_resolution;
pub mod loop_labelling;
pub mod type_checker;
pub mod semantic_analysis;

use std::collections::HashMap;
use loop_labelling::LLStatement;

pub use semantic_analysis::*;
pub use type_checker::*;

use crate::parse::Program;

#[derive(Debug)]
pub struct ValidateResult {
    pub program: Program<LLStatement<TCExpression>, TCExpression>,
    pub symbols: HashMap<String, Symbol>,
}
