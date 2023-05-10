use std::{fmt::Display, rc::{Rc, Weak}};

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    I1,
    I32,
    Ptr(Weak<Type>),
}

thread_local! {
    static VOID_TYPE: Rc<Type> = Rc::new(Type::Void);
    static I32_TYPE: Rc<Type> = Rc::new(Type::I32);
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I1 => 1,
            Type::I32 => 4,
            Type::Ptr(_) => 8,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I1 => 1,
            Type::I32 => 4,
            Type::Ptr(_) => 8,
        }
    }

    pub fn void_type() -> Weak<Type> {
        VOID_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i32_type() -> Weak<Type> {
        I32_TYPE.with(|t| Rc::downgrade(t))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Void => "void",
            Type::I1 => "i1",
            Type::I32 => "i32",
            Type::Ptr(_) => "i32*",
        };
        write!(f, "{}", s)
    }
}

pub trait TypePtrHelper {
    fn get(&self) -> Rc<Type>;
}

impl TypePtrHelper for Weak<Type> {
    fn get(&self) -> Rc<Type> {
        self.upgrade().unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOpType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Ne,
    Assign,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOpType::Add => "add",
            BinaryOpType::Sub => "sub",
            BinaryOpType::Mul => "mul",
            BinaryOpType::Div => "sdiv",
            BinaryOpType::Mod => "srem",
            BinaryOpType::Ne => "icmp ne",
            BinaryOpType::Assign => unreachable!(),
        };
        write!(f, "{}", s)
    }
}