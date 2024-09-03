use crate::*;


#[derive(Clone)]
struct Program {
    function: Function,
}


#[derive(Clone)]
struct Function {
    identifier: String,
    body: Vec<Instruction>,
}


#[derive(Clone)]
enum Instruction {
    Return(Value),
    Unary{op: UnaryOp, src: Value, dst : Value},
}


#[derive(Clone)]
enum Value {
    Constant(i32),
    Var(String),
}

#[derive(Clone)]
enum UnaryOp {
    Complement,
    Negate,
}

fn convert_unop(op : parse::UnaryOperator) -> UnaryOp {
    match op {
        parse::UnaryOperator::Negation => UnaryOp::Negate,
        parse::UnaryOperator::Tilde => UnaryOp::Complement,
    }
}

pub fn emit_tacky(e : parse::Expression, instructions : &mut Vec<Instruction>) -> Value{
    match e {
        parse::Expression::Constant(c) => {
            return Value::Constant(c);
        }
        parse::Expression::Unary(op, inner) => {
            let src = emit_tacky(*inner, instructions);
            let dst_name = "tacky".to_string();
            let dst = Value::Var(dst_name);
            let tacky_op = convert_unop(op);
            instructions.push(Instruction::Unary{op: tacky_op, src, dst: dst.clone()});
            return dst;
        }
    }
} 