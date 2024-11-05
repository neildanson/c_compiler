use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum CodeGenError {
    InvalidConditionCode,
    InvalidBinaryOp,
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CodeGenError::InvalidConditionCode => write!(f, "Invalid Condition Code"),
            CodeGenError::InvalidBinaryOp => write!(f, "Invalid Binary Operation"),
        }
    }
}

#[derive(Debug)]
pub enum SemanticAnalysisError {
    VariableAlreadyDeclared(String),
    VariableNotDeclared(String),
    InvalidLValue,
    NotImplemented,
    InvalidBreak,
    InvalidContinue,
    InvalidBlockItem,
    FunctionNotDeclared(String),
    FunctionAlreadyDeclared(String),
    NestedFunction,
    IncompatibleFunctionDeclarations,
    VariableUsedAsFunctionName,
    StaticFunctionDeclarationFollowsNonStatic(String),
    FunctionRedclaredAsVariable(String),
    ConflictingVariableLinkage(String),
    ConflictingFileScopeVariableDefinitions(String),
    NonStaticFunctionDeclarationInBlock(String),
    InvalidInitializerForFileScopeVariable,
    ExternVariableCannotHaveInitializer,
    NonConstantInitializerForLocalStaticVariable,
    StaticValueNotValidInForLoopInitializer,
    IncompatibleTypesInAssignment,
    IncompatibleTypesInConditional,
    InvalidCastInAssignment
}

impl Display for SemanticAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SemanticAnalysisError::VariableAlreadyDeclared(s) => {
                write!(f, "Variable {} already declared", s)
            }
            SemanticAnalysisError::VariableNotDeclared(s) => {
                write!(f, "Variable {} not declared", s)
            }
            SemanticAnalysisError::InvalidLValue => write!(f, "Invalid LValue"),
            SemanticAnalysisError::NotImplemented => write!(f, "Not Implemented"),
            SemanticAnalysisError::InvalidBreak => write!(f, "Invalid Break"),
            SemanticAnalysisError::InvalidContinue => write!(f, "Invalid Continue"),
            SemanticAnalysisError::InvalidBlockItem => write!(f, "Invalid Block Item"),
            SemanticAnalysisError::FunctionNotDeclared(s) => {
                write!(f, "Function {} not declared", s)
            }
            SemanticAnalysisError::FunctionAlreadyDeclared(s) => {
                write!(f, "Function {} already declared", s)
            }
            SemanticAnalysisError::NestedFunction => write!(f, "Nested Function"),
            SemanticAnalysisError::IncompatibleFunctionDeclarations => {
                write!(f, "Incompatible Function Declarations")
            }
            SemanticAnalysisError::VariableUsedAsFunctionName => {
                write!(f, "Variable used as function name")
            }
            SemanticAnalysisError::StaticFunctionDeclarationFollowsNonStatic(s) => {
                write!(f, "Static function declaration {} follows non-static", s)
            }
            SemanticAnalysisError::FunctionRedclaredAsVariable(s) => {
                write!(f, "Function {} redeclared as variable", s)
            }
            SemanticAnalysisError::ConflictingVariableLinkage(s) => {
                write!(f, "Conflicting variable linkage for {}", s)
            }
            SemanticAnalysisError::ConflictingFileScopeVariableDefinitions(s) => {
                write!(f, "Conflicting file scope variable definitions for {}", s)
            }
            SemanticAnalysisError::NonStaticFunctionDeclarationInBlock(s) => {
                write!(f, "Non-static function declaration {} in block", s)
            }
            SemanticAnalysisError::InvalidInitializerForFileScopeVariable => {
                write!(f, "Invalid initializer for file scope variable")
            }
            SemanticAnalysisError::ExternVariableCannotHaveInitializer => {
                write!(f, "Extern variable cannot have initializer")
            }
            SemanticAnalysisError::NonConstantInitializerForLocalStaticVariable => {
                write!(f, "Non-constant initializer for local static variable")
            }
            SemanticAnalysisError::StaticValueNotValidInForLoopInitializer => {
                write!(f, "Static value not valid in for loop initializer")
            }
            SemanticAnalysisError::IncompatibleTypesInAssignment => {
                write!(f, "Incompatible types in assignment")
            }
            SemanticAnalysisError::IncompatibleTypesInConditional => {
                write!(f, "Incompatible types in conditional")
            }
            SemanticAnalysisError::InvalidCastInAssignment => {
                write!(f, "Invalid cast in assignment")
            }
        }
    }
}

#[derive(Debug)]
pub enum CompilerError {
    IO(std::io::Error),
    Lex,
    Parse(String),
    SemanticAnalysis(SemanticAnalysisError),
    CodeGen(CodeGenError),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::IO(err) => write!(f, "{}", err),
            CompilerError::Lex => write!(f, "Lexing Error"),
            CompilerError::Parse(s) => write!(f, "Parsing Error : {}", s),
            CompilerError::SemanticAnalysis(s) => write!(f, "Semantic Analysis Error : {}", s),
            CompilerError::CodeGen(c) => write!(f, "Code Generation Error : {}", c),
        }
    }
}

impl Error for CompilerError {}
