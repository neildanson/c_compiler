use crate::validate::{InitialValue, StaticInit};

pub type Identifier = String;

#[derive(Debug, PartialEq)]
pub struct Program<S: Clone, E: Clone> {
    pub declarations: Vec<Declaration<S, E>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem<S: Clone, E: Clone> {
    Declaration(Declaration<S, E>),
    Statement(S),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration<S: Clone, E: Clone> {
    pub name: Identifier,
    pub parameters: Vec<(Type, Identifier)>,
    pub body: Option<Vec<BlockItem<S, E>>>,
    pub fun_type: Type,
    pub storage_class: Option<StorageClass>,
    _marker: std::marker::PhantomData<E>,
}

impl<S: Clone, E: Clone> FunctionDeclaration<S, E> {
    pub fn new(
        name: Identifier,
        parameters: Vec<(Type, Identifier)>,
        body: Option<Vec<BlockItem<S, E>>>,
        fun_type: Type,
        storage_class: Option<StorageClass>,
    ) -> Self {
        Self {
            name,
            parameters,
            body,
            fun_type,
            storage_class,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn with_body(&self, body: Option<Vec<BlockItem<S, E>>>) -> Self {
        Self {
            body,
            ..self.clone()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration<S: Clone, E: Clone> {
    Variable(VariableDeclaration<E>),
    Function(FunctionDeclaration<S, E>),
}

impl<S: Clone, E: Clone> Declaration<S, E> {
    pub fn get_name(&self) -> Identifier {
        match self {
            Declaration::Variable(decl) => decl.name.clone(),
            Declaration::Function(decl) => decl.name.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration<E> {
    pub name: Identifier,
    pub init: Option<E>,
    pub storage_class: Option<StorageClass>,
    pub var_type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForInit<E> {
    InitDeclaration(VariableDeclaration<E>),
    InitExpression(Option<E>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<E: Clone> {
    Return(E),
    Expression(E),
    If(E, Box<Statement<E>>, Option<Box<Statement<E>>>),
    Compound(Vec<BlockItem<Statement<E>, E>>),
    Null,
    Break,
    Continue,
    While(E, Box<Statement<E>>),
    DoWhile(Box<Statement<E>>, E),
    For(ForInit<E>, Option<E>, Option<E>, Box<Statement<E>>),
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

impl Constant {
    pub fn get_type(&self) -> Type {
        match self {
            Constant::Int(_) => Type::Int,
            Constant::Long(_) => Type::Long,
        }
    }

    //TODO remove this function
    pub fn i32(&self) -> i32 {
        match self {
            Constant::Int(val) => *val,
            _ => panic!("Invalid conversion"),
        }
    }
}

impl From<Constant> for StaticInit {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Int(val) => StaticInit::IntInit(val),
            Constant::Long(val) => StaticInit::LongInit(val),
        }
    }
}


