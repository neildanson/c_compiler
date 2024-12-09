use std::fmt::{Display, Formatter};

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

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum UnaryOperator {
    Negation,
    Tilde,
    Not,
    PreIncrement,
    PostIncrement,
    PreDecrement,
    PostDecrement,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            UnaryOperator::Negation => write!(f, "-"),
            UnaryOperator::Tilde => write!(f, "~"),
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::PreIncrement => write!(f, "++"),
            UnaryOperator::PostIncrement => write!(f, "++"),
            UnaryOperator::PreDecrement => write!(f, "--"),
            UnaryOperator::PostDecrement => write!(f, "--"),
        }
    }
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

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::ShiftLeft => write!(f, "<<"),
            BinaryOperator::ShiftRight => write!(f, ">>"),
            BinaryOperator::BitwiseAnd => write!(f, "&"),
            BinaryOperator::BitwiseOr => write!(f, "|"),
            BinaryOperator::BitwiseXor => write!(f, "^"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::LessThanOrEqual => write!(f, "<="),
            BinaryOperator::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Long,
    UInt,
    ULong,
    Double, 
    FunType(Vec<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Long => write!(f, "long"),
            Type::UInt => write!(f, "unsigned int"),
            Type::ULong => write!(f, "unsigned long"),
            Type::Double => write!(f, "double"),
            Type::FunType(params, ret) => {
                write!(f, "fun(")?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", ret)
            }
        }
    }
}

impl Type {
    pub fn zero_constant(&self) -> Constant {
        match self {
            Type::Int => Constant::Int(0),
            Type::Long => Constant::Long(0),
            Type::UInt => Constant::UnsignedInt(0),
            Type::ULong => Constant::UnsignedLong(0),
            Type::Double => Constant::Double(0.0),
            Type::FunType(_, _) => panic!("Function type has no zero constant"),
        }
    }

    pub fn is_double(&self) -> bool {
        matches!(self, Type::Double)
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::Int => true,
            Type::Long => true,
            Type::UInt => true,
            Type::ULong => true,
            Type::Double => false,
            Type::FunType(_, _) => false,
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            Type::Int => Some(4),
            Type::Long => Some(8),
            Type::UInt => Some(4),
            Type::ULong => Some(8),
            Type::Double => Some(8),
            Type::FunType(_, _) => None,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Type::Int => true,
            Type::Long => true,
            Type::UInt => false,
            Type::ULong => false,
            Type::Double => panic!("Double type has no sign"), 
            Type::FunType(_, _) => panic!("Function type has no sign"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Constant {
    Int(i32),
    Long(i64),
    UnsignedInt(u32),
    UnsignedLong(u64),
    Double(f64),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Constant::Int(val) => write!(f, "{}", val),
            Constant::Long(val) => write!(f, "{}l", val),
            Constant::UnsignedInt(val) => write!(f, "{}u", val),
            Constant::UnsignedLong(val) => write!(f, "{}ul", val),
            Constant::Double(val) => write!(f, "{}", val),
        }
    }
}

impl Constant {
    pub fn get_type(&self) -> Type {
        match self {
            Constant::Int(_) => Type::Int,
            Constant::Long(_) => Type::Long,
            Constant::UnsignedInt(_) => Type::UInt,
            Constant::UnsignedLong(_) => Type::ULong,
            Constant::Double(_) => Type::Double,
        }
    }

    //i128 can hold all values  of i32, i64, u32, u64
    pub fn as_i128(&self) -> i128 {
        match self {
            Constant::Int(val) => *val as i128,
            Constant::Long(val) => *val as i128,
            Constant::UnsignedInt(val) => *val as i128,
            Constant::UnsignedLong(val) => *val as i128,
            Constant::Double(_) => panic!("Double constant cannot be converted to i128"),
        }
    }
}
