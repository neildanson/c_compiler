pub mod binary_op;
pub mod function;
pub mod instruction;
pub mod operand;
pub mod program;
pub mod register;
pub mod unary_op;
pub mod rewrite_rules;

pub use binary_op::*;
pub use function::*;
pub use instruction::*;
pub use operand::*;
pub use program::*;
pub use register::*;
pub use unary_op::*;

use rewrite_rules::*;

#[cfg(test)]
pub mod tests;
