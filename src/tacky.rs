struct Program {
    function: Function,
}

struct Function {
    identifier: String,
    body: Vec<Instruction>,
}

enum Instruction {
    Return(Value),
    Unary{op: UnaryOp, src: Value, dst : Value},
}

enum Value {
    Constant(i32),
    Var(String),
}

enum UnaryOp {
    Complement,
    Negate,
}
