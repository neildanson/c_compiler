use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Substring {
    s: Arc<str>,
    pos: usize,
}

impl Substring {
    pub fn new(s: Arc<str>, pos: usize) -> Self {
        Self { s, pos }
    }

    pub fn advance(&self, n: usize) -> Self {
        Self::new(Arc::clone(&self.s), self.pos + n)
    }

    pub fn substr(&self, start: usize, end: usize) -> String {
        self.s[self.pos + start..self.pos + end].to_string()
    }
}

impl Deref for Substring {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.s[self.pos..]
    }
}

impl Display for Substring {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self[..], f) //Deref to str
    }
}

impl Debug for Substring {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self[..], f) //Deref to str
    }
}

impl From<&str> for Substring {
    fn from(s: &str) -> Self {
        Self::new(Arc::from(s), 0)
    }
}

impl From<String> for Substring {
    fn from(s: String) -> Self {
        Self::new(Arc::from(s), 0)
    }
}