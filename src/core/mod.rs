use crate::utils::ptr::P;
use anyhow::{anyhow, Error};
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use rpds::HashTrieMap;
use smartstring::alias::String;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub mod closure;
pub mod context;
pub mod conversion;
pub mod expr;
pub mod infer;
pub mod normalizer;
pub mod ty;
pub mod subst;

pub static OP_NAME: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "-", "!", "*", "/", "%", "+", "==", "!=", "<=", ">=", "<", ">",
    ])
});

pub type TyRef = P<Type>;
pub type TyRefs = P<[Type]>;
#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Str,
    Integer,
    Float,
    Bool,
    Var(isize),
    Named(String),
    All(String, TyRef),
    Arrow(TyRefs, TyRef, Option<TyRefs>),
    Variant(P<[(String, TyRefs)]>),
    Tuple(TyRefs),
    Ctor(String, TyRefs),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Operator(pub &'static str);

pub type ExRef = P<Expr>;
pub type ExRefs = P<[Expr]>;
#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Anno(ExRef, TyRef),
    Literal(P<Literal>),
    Var(String),
    Operator(Operator),
    If(ExRef, ExRef, ExRef),
    Abs(P<[(String, Option<TyRef>)]>, ExRef),
    App(ExRef, ExRefs),
    AppClosure(ExRef, ExRefs),
    AppDirectly(ExRef, ExRefs),
    Inj(Option<String>, ExRefs),
    Proj(ExRef, u64),
    Case(ExRef, P<[(String, P<[Option<String>]>, ExRef)]>),
    Let(String, Option<TyRef>, ExRef, ExRef),
    Try(String, ExRef, ExRef),
    Resume(ExRef, ExRef),
    Raise(ExRef, ExRef),
    TAbs(P<[String]>, ExRef),
    TApp(ExRef, TyRefs),
}

#[derive(Debug, Clone)]
pub enum TopBinding {
    Type(String, TyRef),
    Expr(String, TyRef, ExRef),
}

#[derive(Debug, Clone)]
pub struct Subst(String, P<Type>);

#[derive(Debug, Clone)]
pub struct Module(pub P<[TopBinding]>);

#[derive(Debug, Default)]
pub struct Context<K, V>(HashTrieMap<K, V>)
where
    K: Hash + Eq;

macro_rules! binding_group {
    {$($x: ident, $t: ty);*} => {
        #[derive(Debug, Default)]
        pub struct BindingGroup { $($x: Option<$t>),* }
        impl Clone for BindingGroup {
            fn clone(&self) -> Self {
                Self {
                    $($x:self.$x.clone()),*
                }
            }
        }
    };
}

binding_group! {
    name, ();
    var_ty, Option<TyRef>;
    ty_abbr, TyRef;
    closure_subst, ();
    lambda_subst, ();
    op_ty, TyRef
}

#[derive(Debug)]
pub struct ContextGroup {
    ctx_group: Context<String, BindingGroup>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hamt() {
        let mut h0 = HashTrieMap::new();
        h0.insert_mut(1, "1");
        h0.insert_mut(2, "2");
        let h1 = h0.insert(2, "3");
        let h2 = h1.insert(2, "4");
        println!("{h0} {h1} {h2}")
    }

    #[test]
    fn test_to_nameless() {
        use crate::parser::core::*;

        let ty = parse_type(r"(forall (a b) ([a] -> ([b] -> a / c)))").unwrap();
        println!("{ty:#?}");
        let ty = ty.to_locally_nameless();
        println!("{ty:#?}");
    }

    #[test]
    fn test_type_normal_form() {
        use crate::parser::core::*;

        let ty = parse_type(r"(forall (a) (forall (b c) ([b] -> c)))").unwrap();
        let ty = ty.to_locally_nameless();
        println!("{ty:#?}");
        let ty = ty.normalize();
        println!("{ty:#?}");
    }
}
