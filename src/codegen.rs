use crate::ast;


enum Operand {
    Register,//{ reg: u8 },
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
        body.push_str(&format!("    .globl {}\n", self.name));
        body.push_str(&format!("{}:\n", self.name));
        for instruction in &self.body {
            body.push_str(&format!("    {}\n", instruction.to_string()));
        }
        body
    }
}

impl TryFrom<ast::Function> for Function {
    type Error = &'static str;

    fn try_from(ast: ast::Function) -> Result<Self, Self::Error> {
        let mut body = Vec::new();
        for statement in ast.body {
            match statement {
                ast::Statement::Return(expression) => {
                    let operand = match expression {
                        ast::Expression::Int(i) => Operand::Immediate { imm: i },
                        ast::Expression::Identifier(_) => Operand::Register,
                    };
                    body.push(Instruction::Mov { src: operand, dst: Operand::Register });
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
    function : Function,
}



impl Program {
    pub fn to_string(&self) -> String {
        self.function.to_string()
    }
}

impl TryFrom<ast::Program> for Program {
    type Error = &'static str;

    fn try_from(ast: ast::Program) -> Result<Self, Self::Error> {
        let function = Function::try_from(ast.function)?;
        Ok(Program { function })
    }
}