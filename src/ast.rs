use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    pub functions : Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name : String,
    pub body : Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Int(i32),
    Identifier(String),
}


pub fn parse(tokens : &[Token]) -> Program {
    match tokens {
        [] => Program { functions : Vec::new() },
        [Token::Int, Token::Identifier(_), rest @ ..] => parse(rest),
        [Token::Void, Token::Identifier(_), rest @ ..] => parse(rest),
        
        [Token::Return, Token::Constant(c), Token::SemiColon, rest @ ..] => {
            let c = c.parse().unwrap();
            Program {
                functions : vec![Function {
                    name : "main".to_string(),
                    body : vec![Statement::Return(Expression::Int(c))],
                }],
            }
        },
        _ => panic!("Failed to parse {:?}", tokens),
    }

}