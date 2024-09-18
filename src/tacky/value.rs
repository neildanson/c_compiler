#[derive(Clone, Debug)]
pub enum Value {
    Constant(i32),
    Var(String),
}
