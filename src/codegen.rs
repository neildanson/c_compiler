use std::fmt::Display;

use crate::{ast::*, error::CompilerError};

enum Operand {
    Register, //{ reg: u8 },
    Immediate { imm: i32 },
}

impl Display for Operand {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Register => write!(f, "%eax"),
            Operand::Immediate { imm } => write!(f, "${}", imm),
        }
    }
}
enum AsmInstruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl Display for AsmInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AsmInstruction::Mov { src, dst } => write!(f, "movl {}, {}", src, dst),
            AsmInstruction::Ret => write!(f, "ret"),
        }
    }
}

struct AsmFunction {
    name: String,
    body: Vec<AsmInstruction>,
}

impl Display for AsmFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "   .globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.body {
            writeln!(f, "    {}", instruction)?;
        }
        Ok(())
    }
}


impl TryFrom<Function> for AsmFunction {
    type Error = CompilerError;

    fn try_from(ast: Function) -> Result<Self, Self::Error> {
        let mut body = Vec::new();
        for statement in ast.body {
            match statement {
                Statement::Return(expression) => {
                    let operand = match expression {
                        Expression::Int(i) => Operand::Immediate { imm: i },
                        Expression::Identifier(_) => Operand::Register,
                    };
                    body.push(AsmInstruction::Mov {
                        src: operand,
                        dst: Operand::Register,
                    });
                    body.push(AsmInstruction::Ret);
                }
            }
        }
        Ok(AsmFunction {
            name: ast.name,
            body,
        })
    }
}

pub struct AsmProgram {
    function: AsmFunction,
}


impl Display for AsmProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

impl TryFrom<Program> for AsmProgram {
    type Error = CompilerError;

    fn try_from(ast: Program) -> Result<Self, Self::Error> {
        let function = AsmFunction::try_from(ast.function)?;
        Ok(AsmProgram { function })
    }
}
