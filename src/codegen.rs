use std::fmt::Display;

use crate::*;
use crate::error::CompilerError;

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
enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "movl {}, {}", src, dst),
            Instruction::Ret => write!(f, "ret"),
        }
    }
}

struct Function {
    name: String,
    body: Vec<Instruction>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "   .globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.body {
            writeln!(f, "    {}", instruction)?;
        }
        Ok(())
    }
}

impl TryFrom<parse::Function> for Function {
    type Error = CompilerError;

    fn try_from(ast: parse::Function) -> Result<Self, Self::Error> {
        let mut body = Vec::new();
        for statement in ast.body {
            match statement {
                parse::Statement::Return(expression) => {
                    let operand = match expression {
                        parse::Expression::Constant(i) => Operand::Immediate { imm: i },
                        //Expression::Identifier(_) => Operand::Register,
                        parse::Expression::Unary(_, _) => unimplemented!(), // TODO Obvs
                    };
                    body.push(Instruction::Mov {
                        src: operand,
                        dst: Operand::Register,
                    });
                    body.push(Instruction::Ret);
                }
            }
        }
        Ok(Function {
            name: ast.name,
            body,
        })
    }
}

pub struct Program {
    function: Function,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

impl TryFrom<parse::Program> for Program {
    type Error = CompilerError;

    fn try_from(ast: parse::Program) -> Result<Self, Self::Error> {
        let function = Function::try_from(ast.function)?;
        Ok(Program { function })
    }
}
