use std::{fmt::{Display, Debug}, rc::Rc, cell::RefCell};

#[derive(Debug, Clone)]
pub enum TypeKind {
    Void,
    I1,
    I32,
    I64,
    Ptr(Rc<Type>),
    Func(Func),
    Array(Array),
    Const(Rc<Type>),
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

    pub fn base_type(&self) -> Rc<Type> {
        match &self.kind {
            TypeKind::Ptr(ty) => ty.clone(),
            TypeKind::Array(arr) => arr.base_type.clone(),
            TypeKind::Func(func) => func.ret_type.clone(),
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

    pub fn is_array(&self) -> bool {
        matches!(self.kind, TypeKind::Array(_))
    }

    pub fn as_array(&self) -> Array {
        match &self.kind {
            TypeKind::Array(arr) => arr.clone(),
            _ => panic!("not an array"),
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self.kind, TypeKind::Void)
    }

    pub fn is_const(&self) -> bool {
        matches!(self.kind, TypeKind::Const(_))
    }

    pub fn void_type() -> Rc<Type> {
        VOID_TYPE.with(|t| t.clone())
    }

    pub fn i1_type() -> Rc<Type> {
        I1_TYPE.with(|t| t.clone())
    }

    pub fn i32_type() -> Rc<Type> {
        I32_TYPE.with(|t| t.clone())
    }

    pub fn i64_type() -> Rc<Type> {
        I64_TYPE.with(|t| t.clone())
    }

    #[allow(unused)]
    pub fn ptr_to(ty: Rc<Type>) -> Rc<Type> {
        let ty = Rc::new(Type{kind: TypeKind::Ptr(ty), size: 8, align: 8});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn func_type(ret_type: Rc<Type>, params: Vec<(String, Rc<Type>)>) -> Rc<Type> {
        let ty = Rc::new(Type{kind: TypeKind::Func(Func { ret_type, params }), size: 1, align: 1});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn array_of(ty: Rc<Type>, size: isize) -> Rc<Type> {
        let ty = Rc::new(Type{kind: TypeKind::Array(Array { base_type: ty.clone(), len: size }), size: ty.size() * size as isize, align: ty.align()});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn const_of(ty: Rc<Type>) -> Rc<Type> {
        assert!(!matches!(ty.kind, TypeKind::Const(_)));
        let ty = Rc::new(Type{kind: TypeKind::Const(ty.clone()), size: ty.size(), align: ty.align()});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn get_common_type(mut a: Rc<Type>, mut b: Rc<Type>) -> Rc<Type> {
        // todo: a is arr or ptr

        // todo: func ptr

        // todo: float types

        if a.size() < 4 {
            a = Type::i32_type();
        }
        if b.size() < 4 {
            b = Type::i32_type();
        }

        if a.size() != b.size() {
            return if a.size() > b.size() {
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
                write!(f, "{}*", p)
            },
            TypeKind::Func(_func) => unimplemented!(),
            TypeKind::Array(arr) => {
                write!(f, "[{} x {}]", arr.len, arr.base_type)
            },
            TypeKind::Const(ty) => {
                write!(f, "{}", ty)
            },
        }
    }
}

pub trait TypePtrHelper {
    fn get_nocv(&self) -> Rc<Type>;

    fn remove_cv(&self) -> Rc<Type>;
}

impl TypePtrHelper for Rc<Type> {
    fn get_nocv(&self) -> Rc<Type> {
        match &self.kind {
            TypeKind::Const(ty) => ty.clone(),
            _ => self.clone(),
        }
    }

    fn remove_cv(&self) -> Rc<Type> {
        match &self.kind {
            TypeKind::Const(ty) => ty.clone(),
            TypeKind::Array(arr) => Type::array_of(arr.base_type.remove_cv(), arr.len),
            _ => self.clone(),
        }
    }
}

pub trait TypePtrCompare {
    fn is_same_as(&self, other: &Self) -> bool;
}

impl TypePtrCompare for Rc<Type> {
    fn is_same_as(&self, other: &Self) -> bool {
        if self.is_ptr() && other.is_ptr() {
            self.base_type().is_same_as(&other.base_type())
        } else if self.is_const() && other.is_const() {
            self.get_nocv().is_same_as(&other.get_nocv())
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
    pub ret_type: Rc<Type>,
    pub params: Vec<(String, Rc<Type>)>,
}

#[derive(Debug, Clone)]
pub struct Array {
    pub base_type: Rc<Type>,
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