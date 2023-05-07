use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    I32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I32 => 4,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I32 => 4,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Void => "void",
            Type::I32 => "i32",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOpType::Add => "add",
            BinaryOpType::Sub => "sub",
            BinaryOpType::Mul => "mul",
            BinaryOpType::Div => "sdiv",
            BinaryOpType::Mod => "srem",
        };
        write!(f, "{}", s)
    }
}