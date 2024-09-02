use crate::{ast::*, error::CompilerError};

enum Operand {
    Register, //{ reg: u8 },
    Immediate { imm: i32 },
}

impl Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Register => "%eax".to_string(),
            Operand::Immediate { imm } => format!("${}", imm),
        }
    }
}
enum AsmInstruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl AsmInstruction {
    fn to_string(&self) -> String {
        match self {
            AsmInstruction::Mov { src, dst } => {
                format!("movl {}, {}", src.to_string(), dst.to_string())
            }
            AsmInstruction::Ret => "ret".to_string(),
        }
    }
}

struct AsmFunction {
    name: String,
    body: Vec<AsmInstruction>,
}

impl AsmFunction {
    fn to_string(&self) -> String {
        let mut body = String::new();
        body.push_str(&format!("    .globl {}\n", self.name));
        body.push_str(&format!("{}:\n", self.name));
        for instruction in &self.body {
            body.push_str(&format!("    {}\n", instruction.to_string()));
        }
        body
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

impl AsmProgram {
    pub fn to_string(&self) -> String {
        self.function.to_string()
    }
}

impl TryFrom<Program> for AsmProgram {
    type Error = CompilerError;

    fn try_from(ast: Program) -> Result<Self, Self::Error> {
        let function = AsmFunction::try_from(ast.function)?;
        Ok(AsmProgram { function })
    }
}
