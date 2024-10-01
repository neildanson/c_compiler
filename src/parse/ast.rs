#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Vec<BlockItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<BlockItem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDefinition),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub value: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForInit {
    InitDeclaration(VariableDeclaration),
    InitExpression(Option<Expression>),
}

type LoopIdentifier = Option<String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<BlockItem>),
    Null,
    Break(LoopIdentifier),
    Continue(LoopIdentifier),
    While(Expression, Box<Statement>, LoopIdentifier),
    DoWhile(Box<Statement>, Expression, LoopIdentifier),
    For(
        ForInit,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
        LoopIdentifier,
    ),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Constant(i32),
    Var(String),
    Unary(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Negation,
    Tilde,
    Not,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}
