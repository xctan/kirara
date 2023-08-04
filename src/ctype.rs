use std::{fmt::{Display, Debug}, rc::Rc, cell::RefCell};

#[derive(Debug, Clone)]
pub enum TypeKind {
    Void,
    I1,
    I32,
    I64,
    F32,
    Ptr(Rc<Type>),
    Func(Func),
    Array(Array),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub size: isize,
    pub align: usize,
    pub is_const: bool,
}

thread_local! {
    static TYPES: RefCell<Vec<Rc<Type>>> = RefCell::new(vec![]);

    static VOID_TYPE: Rc<Type> = Type::new(TypeKind::Void, 0, 0);
    static I1_TYPE: Rc<Type> = Type::new(TypeKind::I1, 1, 1);
    static I32_TYPE: Rc<Type> = Type::new(TypeKind::I32, 4, 4);
    static I64_TYPE: Rc<Type> = Type::new(TypeKind::I64, 8, 8);
    static F32_TYPE: Rc<Type> = Type::new(TypeKind::F32, 4, 4);
}

impl Type {
    pub fn new(kind: TypeKind, size: isize, align: usize) -> Rc<Type> {
        let ty = Rc::new(Type{kind, size, align, is_const: false});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn new_const(kind: TypeKind, size: isize, align: usize) -> Rc<Type> {
        let ty = Rc::new(Type{kind, size, align, is_const: true});
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

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

    pub fn is_int(&self) -> bool {
        matches!(self.kind, TypeKind::I1 | TypeKind::I32 | TypeKind::I64 | TypeKind::Ptr(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, TypeKind::F32)
    }

    // pub fn is_const(&self) -> bool {
    //     self.is_const
    // }

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

    pub fn f32_type() -> Rc<Type> {
        F32_TYPE.with(|t| t.clone())
    }

    #[allow(unused)]
    pub fn ptr_to(ty: Rc<Type>) -> Rc<Type> {
        let ty = Type::new(TypeKind::Ptr(ty), 8, 8);
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn func_type(ret_type: Rc<Type>, params: Vec<(String, Rc<Type>)>) -> Rc<Type> {
        let ty = Type::new(TypeKind::Func(Func { ret_type, params }), 1, 1);
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn array_of(ty: Rc<Type>, size: isize) -> Rc<Type> {
        // negative len is reserved for flexible array member
        let ty = Type::new(TypeKind::Array(Array { base_type: ty.clone(), len: size }), ty.size() * size as isize, ty.align());
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn const_of(ty: Rc<Type>) -> Rc<Type> {
        let ty = Type::new_const(ty.kind.clone(), ty.size(), ty.align());
        // TYPES.with(|t| t.borrow_mut().push(ty.clone()));
        ty
    }

    pub fn decay(&self) -> Rc<Type> {
        match &self.kind {
            TypeKind::Array(arr) => Self::ptr_to(arr.base_type.clone()),
            _ => panic!("not an array"),
        }
    }

    pub fn get_common_type(mut a: Rc<Type>, mut b: Rc<Type>) -> Rc<Type> {
        // todo: a is arr or ptr

        // func ptr
        if a.is_function() {
            return Self::ptr_to(a);
        }
        if b.is_function() {
            return Self::ptr_to(b);
        }

        // float types
        // long double?
        // double?
        if matches!(a.kind, TypeKind::F32) || matches!(b.kind, TypeKind::F32) {
            return Type::f32_type();
        }

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
            // TypeKind::F32 => write!(f, "f32"),
            TypeKind::F32 => write!(f, "float"),
            // TypeKind::Ptr(p) => {
            //     write!(f, "{}*", p)
            // },
            TypeKind::Ptr(_p) => write!(f, "ptr"),
            TypeKind::Func(_func) => unimplemented!(),
            TypeKind::Array(arr) => {
                write!(f, "[{} x {}]", arr.len, arr.base_type)
            },
        }
    }
}

pub trait TypePtrCompare {
    fn is_same_as(&self, other: &Self) -> bool;
}

impl TypePtrCompare for Rc<Type> {
    fn is_same_as(&self, other: &Self) -> bool {
        *self == *other
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.is_ptr() && other.is_ptr() {
            self.base_type() == other.base_type()
        } else if self.is_array() && other.is_array() {
            self.base_type() == other.base_type()
        } else {
            matches!(
                (&self.kind, &other.kind), 
                (&TypeKind::Void, &TypeKind::Void) | 
                (&TypeKind::I1, &TypeKind::I1) | 
                (&TypeKind::I32, &TypeKind::I32) |
                (&TypeKind::I64, &TypeKind::I64) |
                (&TypeKind::F32, &TypeKind::F32)
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
#[allow(unused)]
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
    Xor,
    And,
    Shr,
    Shl,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOpType::Add => "add",
            BinaryOpType::Sub => "sub",
            BinaryOpType::Mul => "mul",
            BinaryOpType::Div => "sdiv",
            BinaryOpType::Mod => "srem",
            BinaryOpType::Ne => "???ne",
            BinaryOpType::Eq => "???eq",
            BinaryOpType::Lt => "???lt",
            BinaryOpType::Le => "???le",
            BinaryOpType::Gt => "???gt",
            BinaryOpType::Ge => "???ge",
            BinaryOpType::Xor => "xor",
            BinaryOpType::And => "and",
            BinaryOpType::Shr => "??shr",
            BinaryOpType::Shl => "shl",
            BinaryOpType::Assign |
            BinaryOpType::Index |
            BinaryOpType::LogOr |
            BinaryOpType::LogAnd => unreachable!(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpType {
    Neg,
    LogNot,
}

#[derive(Debug, Clone, Copy)]
#[allow(unused)]
pub enum Linkage {
    Static,
    Global,
    Extern,
}