//! # AST context management
//! 
//! This mod uses dirty and unsafe code to manage the AST context as a global object.

use std::rc::Weak;

use crate::{token::TokenSpan, ctype::Type};

use super::{AstContext, ObjectId, AstObject, ScopeVar};


static mut CTX: Option<AstContext> = None;

pub fn enter_scope() {
    unsafe {
        CTX.as_mut().unwrap().enter_scope();
    }
}

pub fn leave_scope() {
    unsafe {
        CTX.as_mut().unwrap().leave_scope();
    }
}

pub fn gen_unique_name(prefix: &str) -> String {
    unsafe {
        CTX.as_mut().unwrap().gen_unique_name(prefix)
    }
}

pub fn new_local_var_with_token(id: TokenSpan, ty: Weak<Type>) -> ObjectId {
    let instance = unsafe {
        CTX.as_mut().unwrap()
    };
    let obj_id = instance.new_local_var(id.as_str(), ty);
    let obj = instance.get_object_mut(obj_id).unwrap();
    obj.token = id.as_range();
    obj_id
}

pub fn new_global_var(id: &str, ty: Weak<Type>) -> ObjectId {
    let instance = unsafe {
        CTX.as_mut().unwrap()
    };
    instance.new_global_var(id, ty)
}

pub fn get_object(id: ObjectId) -> Option<&'static AstObject> {
    unsafe {
        CTX.as_mut().unwrap().get_object(id)
    }
}

pub fn get_object_mut(id: ObjectId) -> Option<&'static mut AstObject> {
    unsafe {
        CTX.as_mut().unwrap().get_object_mut(id)
    }
}

pub fn find_var(id: &str) -> Option<ScopeVar> {
    unsafe {
        CTX.as_mut().unwrap().find_var(id)
    }
}

pub fn init_context() {
    unsafe {
        CTX.replace(AstContext::new());
    }
}

pub fn take_context() -> AstContext {
    unsafe {
        CTX.take().unwrap()
    }
}

pub fn get_context_locals_mut() -> &'static mut Vec<ObjectId> {
    unsafe {
        &mut CTX.as_mut().unwrap().locals
    }
}