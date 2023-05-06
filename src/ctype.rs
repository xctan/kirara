#[derive(Debug, Clone)]
pub enum Type {
    Void,
    I32,
}

#[derive(Debug, Clone)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}