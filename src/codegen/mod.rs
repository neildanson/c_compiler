pub mod register;
pub mod unary_op;
pub mod binary_op;
pub mod operand;
pub mod instruction;
pub mod function;
pub mod program;

pub use register::*;
pub use unary_op::*;
pub use binary_op::*;
pub use operand::*;
pub use instruction::*;
pub use function::*;
pub use program::*;


#[cfg(test)]
pub mod tests;