use super::{AssemblyType, Function, StaticVariable, TopLevel};
use crate::{error::CompilerError, tacky::{self}, validate::{Symbol, Value}};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, PartialEq)]
pub struct Program {
    top_level: Vec<TopLevel>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for top_level in &self.top_level {
            write!(f, "{}", top_level)?;
            writeln!(f)?;
        }
        if cfg!(target_os = "linux") {
            writeln!(f, ".section .note.GNU-stack,\"\",@progbits")?;
        }

        Ok(())
    }
}

impl TryFrom<tacky::Program> for Program {
    type Error = CompilerError;
    fn try_from(ast: tacky::Program) -> Result<Self, Self::Error> {
        let mut top_level = Vec::new();

        for tl in ast.top_level {
            match tl {
                tacky::TopLevel::Function(f) => {
                    top_level.push(TopLevel::Function(Function::try_from(f)?));
                }
                tacky::TopLevel::StaticVariable(s) => {
                    top_level.push(TopLevel::StaticVariable(StaticVariable::try_from(s)?));
                }
            }
        }

        Ok(Program { top_level })
    }
}

#[derive(Debug)]
pub enum AsmSymTabEntry {
    ObjEntry(AssemblyType, bool), //Static
    FunEntry(bool) //Defined
}

impl AsmSymTabEntry {
    fn from_symbol(symbol:&Symbol) -> Self {
        match symbol {
            Symbol::FunType(a,_,_) => AsmSymTabEntry::FunEntry(a.defined),
            Symbol::Value(Value::Static(_,ty)) => AsmSymTabEntry::ObjEntry(ty.into(), true), //always true?
            Symbol::Value(Value::Local(ty)) => AsmSymTabEntry::ObjEntry(ty.into(), false)
        }
    }
}


impl Program {
    pub fn fixup(&mut self, symbols: &HashMap<String, Symbol>) {
        let symtab : HashMap<String, AsmSymTabEntry> =
        symbols.iter().map(|(k,v)| {
            (k.clone(), AsmSymTabEntry::from_symbol(v))
        }).collect();
        for top_level in &mut self.top_level {
            match top_level {
                TopLevel::Function(f) => f.fixup(&symtab),
                TopLevel::StaticVariable(_) => {}
            }
        }
    }
}
