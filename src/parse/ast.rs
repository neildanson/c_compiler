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
    pub parameters: Vec<(Type, Identifier)>,
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
    Var(Identifier, Option<Type>),
    Unary(UnaryOperator, Box<Expression>, Option<Type>),
    BinOp(
        BinaryOperator,
        Box<Expression>,
        Box<Expression>,
        Option<Type>,
    ),
    Assignment(Box<Expression>, Box<Expression>, Option<Type>),
    Conditional(
        Box<Expression>,
        Box<Expression>,
        Box<Expression>,
        Option<Type>,
    ),
    FunctionCall(Identifier, Vec<Expression>, Option<Type>),
    Cast(Type, Box<Expression>),
    Constant(Constant),
}

impl Expression {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expression::Var(_, t) => t.clone(),
            Expression::Unary(_, _, t) => t.clone(),
            Expression::BinOp(_, _, _, t) => t.clone(),
            Expression::Assignment(_, _, t) => t.clone(),
            Expression::Conditional(_, _, _, t) => t.clone(),
            Expression::FunctionCall(_, _, t) => t.clone(),
            Expression::Cast(t, _) => Some(t.clone()),
            Expression::Constant(c) => Some(c.get_type()),
        }
    }
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

impl Constant {
    pub fn get_type(&self) -> Type {
        match self {
            Constant::Int(_) => Type::Int,
            Constant::Long(_) => Type::Long,
        }
    }
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
