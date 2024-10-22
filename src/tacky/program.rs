use std::collections::HashMap;

use crate::validate::Symbol;

use super::TopLevel;

#[derive(Clone, Debug)]
pub struct Program {
    pub top_level: Vec<TopLevel>,
    pub symbols: HashMap<String, Symbol>,
}
