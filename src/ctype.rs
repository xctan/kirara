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
    static I1_TYPE: Rc<Type> = Rc::new(Type::I1);
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

    pub fn base_type(&self) -> Weak<Type> {
        match self {
            Type::Ptr(ty) => ty.clone(),
            _ => panic!("not applicable"),
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    pub fn void_type() -> Weak<Type> {
        VOID_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i32_type() -> Weak<Type> {
        I32_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i1_type() -> Weak<Type> {
        I1_TYPE.with(|t| Rc::downgrade(t))
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

pub trait TypePtrCompare {
    fn same_as(self, other: Self) -> bool;
}

impl TypePtrCompare for Weak<Type> {
    fn same_as(self, other: Self) -> bool {
        if self.get().is_ptr() && other.get().is_ptr() {
            self.get().base_type().same_as(other.get().base_type())
        } else {
            matches!(
                (&*self.get(), &*other.get()), 
                (&Type::Void, &Type::Void) | 
                (&Type::I1, &Type::I1) | 
                (&Type::I32, &Type::I32)
            )
        }
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
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
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
            BinaryOpType::Eq => "icmp eq",
            BinaryOpType::Lt => "icmp slt",
            BinaryOpType::Le => "icmp sle",
            BinaryOpType::Gt => "icmp sgt",
            BinaryOpType::Ge => "icmp sge",
            BinaryOpType::Assign => unreachable!(),
        };
        write!(f, "{}", s)
    }
}