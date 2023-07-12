use std::{fmt::{Display, Debug}, rc::{Rc, Weak}, cell::RefCell, unimplemented};

#[derive(Debug, Clone)]
pub enum TypeKind {
    Void,
    I1,
    I32,
    I64,
    Ptr(Weak<Type>),
    Func(Func),
    Array(Array),
    Const(Weak<Type>),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub size: isize,
    pub align: usize,
}

thread_local! {
    static TYPES: RefCell<Vec<Rc<Type>>> = RefCell::new(vec![]);

    static VOID_TYPE: Rc<Type> = Rc::new(Type{kind: TypeKind::Void, size: 0, align: 0});
    static I1_TYPE: Rc<Type> = Rc::new(Type{kind: TypeKind::I1, size: 1, align: 1});
    static I32_TYPE: Rc<Type> = Rc::new(Type{kind: TypeKind::I32, size: 4, align: 4});
    static I64_TYPE: Rc<Type> = Rc::new(Type{kind: TypeKind::I64, size: 8, align: 8});
}

impl Type {
    pub fn size(&self) -> isize {
        self.size
    }

    pub fn align(&self) -> usize {
        self.align
    }

    pub fn base_type(&self) -> Weak<Type> {
        match &self.kind {
            TypeKind::Ptr(ty) => ty.clone(),
            TypeKind::Array(arr) => arr.base_type.clone(),
            _ => panic!("not applicable"),
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.kind, TypeKind::Ptr(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, TypeKind::Func(_))
    }

    pub fn as_function(&self) -> Func {
        match &self.kind {
            TypeKind::Func(f) => f.clone(),
            _ => panic!("not a function"),
        }
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
        let ty = Rc::new(Type{kind: TypeKind::Ptr(ty), size: 8, align: 8});
        TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        Rc::downgrade(&ty)
    }

    pub fn func_type(ret_type: Weak<Type>, params: Vec<(String, Weak<Type>)>) -> Weak<Type> {
        let ty = Rc::new(Type{kind: TypeKind::Func(Func { ret_type, params }), size: 1, align: 1});
        TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        Rc::downgrade(&ty)
    }

    pub fn array_of(ty: Weak<Type>, size: isize) -> Weak<Type> {
        let ty = Rc::new(Type{kind: TypeKind::Array(Array { base_type: ty.clone(), len: size }), size: ty.get().size() * size as isize, align: ty.get().align()});
        TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        Rc::downgrade(&ty)
    }

    pub fn const_of(ty: Weak<Type>) -> Weak<Type> {
        assert!(!matches!(ty.get().kind, TypeKind::Const(_)));
        let ty = Rc::new(Type{kind: TypeKind::Const(ty.clone()), size: ty.get().size(), align: ty.get().align()});
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
        match &self.kind {
            TypeKind::Void => write!(f, "void"),
            TypeKind::I1 => write!(f, "i1"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::Ptr(p) => {
                write!(f, "{}*", p.get().base_type().get())
            },
            TypeKind::Func(_func) => unimplemented!(),
            TypeKind::Array(arr) => {
                write!(f, "[{} x {}]", arr.len, arr.base_type.get())
            },
            TypeKind::Const(ty) => {
                write!(f, "{}", ty.get())
            },
        }
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
                (&self.get().kind, &other.get().kind), 
                (&TypeKind::Void, &TypeKind::Void) | 
                (&TypeKind::I1, &TypeKind::I1) | 
                (&TypeKind::I32, &TypeKind::I32)
            )
        }
    }
}

impl TypePtrCompare for Rc<Type> {
    fn is_same_as(&self, other: &Self) -> bool {
        if self.is_ptr() && other.is_ptr() {
            self.base_type().is_same_as(&other.base_type())
        } else {
            matches!(
                (&self.kind, &other.kind), 
                (&TypeKind::Void, &TypeKind::Void) | 
                (&TypeKind::I1, &TypeKind::I1) | 
                (&TypeKind::I32, &TypeKind::I32)
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub ret_type: Weak<Type>,
    pub params: Vec<(String, Weak<Type>)>,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub base_type: Weak<Type>,
    pub len: isize,
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
    Index,
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
            BinaryOpType::Index |
            BinaryOpType::LogOr |
            BinaryOpType::LogAnd => unreachable!(),
        };
        write!(f, "{}", s)
    }
}