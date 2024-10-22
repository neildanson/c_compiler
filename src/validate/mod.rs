mod identifier_resolution;
mod loop_labelling;
mod type_checker;

pub mod semantic_analysis;
pub use type_checker::*;
pub use semantic_analysis::*;
