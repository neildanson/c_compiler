use crate::ast;


enum Operand {
    Register,//{ reg: u8 },
    Immediate { imm: i32 },
}

impl Operand {
    fn to_string(&self) -> String {
        match self {
            Operand::Register => "eax".to_string(),
            Operand::Immediate { imm } => format!("${}", imm.to_string()),
        }
    }
}
enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl Instruction {
    fn to_string(&self) -> String {
        match self {
            Instruction::Mov { src, dst } => format!("movl {}, {}", src.to_string(), dst.to_string()),
            Instruction::Ret => "ret".to_string(),
        }
    }
}

struct Function {
    name: String,
    body: Vec<Instruction>,
}

impl Function {
    fn to_string(&self) -> String {
        let mut body = String::new();
        for instruction in &self.body {
            body.push_str(&format!("    {}\n", instruction.to_string()));
        }
        body
    }
}


pub struct Program {
    function : Function,
}



impl Program {
    pub fn to_string(&self) -> String {
        format!(".globl main\nmain:\n{}", self.function.to_string())
    }
}

impl TryFrom<ast::Program> for Program {
    type Error = &'static str;

    fn try_from(ast: ast::Program) -> Result<Self, Self::Error> {
        let function = Function {
            name: "main".to_string(),
            body: vec![
                Instruction::Mov {
                    src: Operand::Immediate { imm: 2 },
                    dst: Operand::Register,
                },
                Instruction::Ret,
            ],
        };
        Ok(Program { function })
    }
}