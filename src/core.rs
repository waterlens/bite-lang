use once_cell::sync::Lazy;
use smartstring::alias::String;
use std::collections::HashSet;

pub(crate) static OP_NAME: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "-", "!", "*", "/", "%", "+", "==", "!=", "<=", ">=", "<", ">",
    ])
});

pub(crate) type TyRef = Box<Type>;
#[derive(Debug, Clone)]
pub(crate) enum Type {
    Unit,
    Str,
    Integer,
    Bool,
    Var(i32, i32),
    Named(String),
    All(Vec<String>, TyRef),
    Arrow(TyRef, TyRef, Option<TyRef>),
    Variant(Vec<(String, Vec<Type>)>),
    Tuple(Vec<Type>),
    Ctor(String, Vec<Type>),
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub(crate) struct Operator {
    pub(crate) name: &'static str,
}

pub(crate) type ExRef = Box<Expr>;
#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Unit,
    Anno(ExRef, TyRef),
    Literal(Box<Literal>),
    Var(String),
    Operator(Operator),
    If(ExRef, ExRef, ExRef),
    Abs(Vec<(String, Type)>, ExRef),
    App(ExRef, Vec<Expr>),
    Inj(Option<String>, Vec<Expr>),
    Proj(ExRef, usize),
    Case(ExRef, Vec<(String, Vec<Option<String>>, Expr)>),
    Let(Option<String>, ExRef, ExRef),
    Try(ExRef, Option<String>, ExRef),
    Resume(ExRef, ExRef),
    Raise(ExRef, ExRef),
    TAbs(Vec<String>, ExRef),
    TApp(ExRef, Vec<Type>),
}

pub(crate) enum TopBinding {
    TypeBinding(String, Type),
    ExprBinding(Option<String>, Expr)
}

pub(crate) struct Module(pub(crate) Vec<TopBinding>);
