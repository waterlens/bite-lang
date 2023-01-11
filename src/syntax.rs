use smartstring::alias::String;
use std::collections::HashMap;

pub(crate) struct Module {
    pub(crate) fn_def: Vec<FnDef>,
    pub(crate) type_def: Vec<TypeDef>,
    pub(crate) eff_def: Vec<EffDef>,
}

pub(crate) struct FnDef {
    pub(crate) name: String,
    pub(crate) parameter: Vec<Parameter>,
    pub(crate) annotation: Option<Type>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) attr: Option<Attribute>,
}

pub(crate) struct TypeDef {
    pub(crate) name: String,
    pub(crate) parameter: Vec<String>,
    pub(crate) body: TypeDefBody,
    pub(crate) attr: Option<Attribute>,
}

pub(crate) enum TypeDefBody {
    Synonym(Box<Type>),
    Variant(Vec<Ctor>),
}

pub(crate) struct EffDef {
    pub(crate) name: String,
    pub(crate) parameter: Vec<Parameter>,
    pub(crate) annotation: Option<Type>,
    pub(crate) attr: Option<Attribute>,
}

pub(crate) struct Ctor {
    pub(crate) name: Option<String>,
    pub(crate) fields: Option<Vec<Type>>,
}

pub(crate) enum Type {
    Named(String),
    Quoted(String),
    Tuple(Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
    App(Box<Type>, Box<Type>),
}

pub(crate) type Operator = crate::core::Operator;

pub(crate) enum Expr {
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
pub(crate) type Literal = crate::core::Literal;

pub(crate) enum Pattern {
    Identifier(String),
    Wildcard,
    Ctor(Option<String>, Vec<Pattern>),
}

pub(crate) struct Parameter(pub(crate) Box<Pattern>, pub(crate) Box<Type>);

pub(crate) enum Stmt {
    Let {
        name: Pattern,
        ty: Option<Box<Type>>,
        init: Option<Box<Expr>>,
    },
    Expr(Expr),
}

pub(crate) struct Attribute(pub(crate) HashMap<String, Vec<Expr>>);
