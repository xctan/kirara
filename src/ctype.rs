use std::{fmt::{Display, Debug}, rc::{Rc, Weak}, cell::RefCell, unimplemented};

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    I1,
    I32,
    I64,
    Ptr(Weak<Type>),
    Func(Func),
}

thread_local! {
    static TYPES: RefCell<Vec<Rc<Type>>> = RefCell::new(vec![]);

    static VOID_TYPE: Rc<Type> = Rc::new(Type::Void);
    static I1_TYPE: Rc<Type> = Rc::new(Type::I1);
    static I32_TYPE: Rc<Type> = Rc::new(Type::I32);
    static I64_TYPE: Rc<Type> = Rc::new(Type::I64);
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I1 => 1,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::Ptr(_) => 8,
            Type::Func(_) => 1,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I1 => 1,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::Ptr(_) => 8,
            Type::Func(_) => 1,
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

    pub fn is_function(&self) -> bool {
        matches!(self, Type::Func(_))
    }

    pub fn void_type() -> Weak<Type> {
        VOID_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i1_type() -> Weak<Type> {
        I1_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i32_type() -> Weak<Type> {
        I32_TYPE.with(|t| Rc::downgrade(t))
    }

    pub fn i64_type() -> Weak<Type> {
        I64_TYPE.with(|t| Rc::downgrade(t))
    }

    #[allow(unused)]
    pub fn ptr_to(ty: Weak<Type>) -> Weak<Type> {
        let ty = Rc::new(Type::Ptr(ty));
        TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        Rc::downgrade(&ty)
    }

    pub fn func_type(ret_type: Weak<Type>, params: Vec<(String, Weak<Type>)>) -> Weak<Type> {
        let ty = Rc::new(Type::Func(Func { ret_type, params }));
        TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        Rc::downgrade(&ty)
    }

    pub fn get_common_type(mut a: Weak<Type>, mut b: Weak<Type>) -> Weak<Type> {
        // todo: a is arr or ptr

        // todo: func ptr

        // todo: float types

        if a.get().size() < 4 {
            a = Type::i32_type();
        }
        if b.get().size() < 4 {
            b = Type::i32_type();
        }

        if a.get().size() != b.get().size() {
            return if a.get().size() > b.get().size() {
                a
            } else {
                b
            };
        }

        // todo: unsigned type
        
        a
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Type::Void => "void",
            Type::I1 => "i1",
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::Ptr(p) => {
                write!(f, "{}", p.get().base_type().get())?;
                "*"
            },
            Type::Func(func) => unimplemented!(),
        };
        write!(f, "{}", s)
    }
}

pub trait TypePtrHelper {
    fn get(&self) -> Rc<Type>;
    fn is_function(&self) -> bool;
}

impl TypePtrHelper for Weak<Type> {
    fn get(&self) -> Rc<Type> {
        self.upgrade().unwrap()
    }

    fn is_function(&self) -> bool {
        self.get().is_function()
    }
}

pub trait TypePtrCompare {
    fn is_same_as(&self, other: &Self) -> bool;
}

impl TypePtrCompare for Weak<Type> {
    fn is_same_as(&self, other: &Self) -> bool {
        if self.get().is_ptr() && other.get().is_ptr() {
            self.get().base_type().is_same_as(&other.get().base_type())
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

#[derive(Debug, Clone)]
pub struct Func {
    pub ret_type: Weak<Type>,
    pub params: Vec<(String, Weak<Type>)>,
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
    LogOr,
    LogAnd,
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
            BinaryOpType::Assign |
            BinaryOpType::LogOr |
            BinaryOpType::LogAnd => unreachable!(),
        };
        write!(f, "{}", s)
    }
}