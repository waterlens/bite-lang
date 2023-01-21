use smartstring::alias::String;
use std::collections::HashMap;

pub struct Module {
    pub fn_def: Vec<FnDef>,
    pub type_def: Vec<TypeDef>,
    pub eff_def: Vec<EffDef>,
}

pub struct FnDef {
    pub name: String,
    pub parameter: Vec<Parameter>,
    pub annotation: Option<Type>,
    pub body: Vec<Stmt>,
    pub attr: Option<Attribute>,
}

pub struct TypeDef {
    pub name: String,
    pub parameter: Vec<String>,
    pub body: TypeDefBody,
    pub attr: Option<Attribute>,
}

pub enum TypeDefBody {
    Synonym(Box<Type>),
    Variant(Vec<Ctor>),
}

pub struct EffDef {
    pub name: String,
    pub parameter: Vec<Parameter>,
    pub annotation: Option<Type>,
    pub attr: Option<Attribute>,
}

pub struct Ctor {
    pub name: Option<String>,
    pub fields: Option<Vec<Type>>,
}

pub enum Type {
    Named(String),
    Quoted(String),
    Tuple(Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
    App(Box<Type>, Box<Type>),
}

pub type Operator = crate::core::Operator;

pub enum Expr {
    Literal(Literal),
    Variable(String),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Try(Box<Expr>, Vec<(Pattern, Expr)>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Unary(Operator, Box<Expr>),
    Binary(Operator, Box<Expr>, Box<Expr>),
    FieldCall(Box<Expr>, String, Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Ctor(Box<Ctor>, Vec<Expr>),
    Tuple(Vec<Expr>),
    Attr(Attribute),
    Resume(Box<Expr>),
    Raise(Box<Expr>),
    Block(Vec<Stmt>),
}
pub type Literal = crate::core::Literal;

pub enum Pattern {
    Identifier(String),
    Wildcard,
    Ctor(Option<String>, Vec<Pattern>),
}

pub struct Parameter(pub Box<Pattern>, pub Box<Type>);

pub enum Stmt {
    Let {
        name: Pattern,
        ty: Option<Box<Type>>,
        init: Option<Box<Expr>>,
    },
    Expr(Expr),
}

pub struct Attribute(pub HashMap<String, Vec<Expr>>);
