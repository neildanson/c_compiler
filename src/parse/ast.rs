pub type Identifier = String;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Option<Vec<BlockItem>>,
    pub fun_type: Type,
    pub storage_class: Option<StorageClass>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

impl Declaration {
    pub fn get_name(&self) -> Identifier {
        match self {
            Declaration::Variable(decl) => decl.name.clone(),
            Declaration::Function(decl) => decl.name.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
    pub var_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForInit {
    InitDeclaration(VariableDeclaration),
    InitExpression(Option<Expression>),
}

type LoopIdentifier = Option<Identifier>;

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
    Var(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    Cast(Type, Box<Expression>),
    Constant(Constant),
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

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Long, 
    FunType(Vec<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Int(i32),
    Long(i64),
}

//TODO remove this?
impl From<Constant> for i32 {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Int(val) => val,
            _ => panic!("Expected int constant"),
        }
    }
}