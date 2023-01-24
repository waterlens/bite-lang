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

pub(crate) mod closure;
pub(crate) mod context;
pub(crate) mod conversion;
pub(crate) mod normalizer;
pub(crate) mod ty;
pub(crate) mod expr;

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
    Bool,
    Var(isize),
    Named(String),
    All(String, TyRef),
    Arrow(TyRefs, TyRef, Option<TyRef>),
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
    Literal(Box<Literal>),
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
    Expr(String, ExRef),
}

#[derive(Debug, Clone)]
pub struct Module(pub P<[TopBinding]>);

#[derive(Debug)]
pub struct Context<K, V>(HashTrieMap<K, V>)
where
    K: Hash + Eq;

pub enum Binding {
    Name,
    VarTy(Option<TyRef>),
    TyAbbr(TyRef),
    ClosureAbbr(ExRef),
    LambdaAbbr(ExRef),
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
