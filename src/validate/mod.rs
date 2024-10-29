mod identifier_resolution;
mod loop_labelling;
mod type_checker;

pub mod semantic_analysis;
use std::collections::HashMap;

pub use semantic_analysis::*;
pub use type_checker::*;

use crate::parse::Program;

#[derive(Debug)]
pub struct ValidateResult {
    pub program: Program,
    pub symbols: HashMap<String, Symbol>,
}