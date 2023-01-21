use itertools::Itertools;
use smartstring::alias::String;
use std::fmt::Display;

#[derive(Debug)]
pub enum Sexp<S: AsRef<str>> {
    Bool(bool),
    Integer(i64),
    Float(f64),
    Str(String),
    Ident(S),
    Op(S),
    List(Vec<Sexp<S>>),
}

impl<S: AsRef<str>> Display for Sexp<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexp::Bool(x) => write!(f, "{x}"),
            Sexp::Integer(x) => write!(f, "{x}"),
            Sexp::Float(x) => write!(f, "{x}"),
            Sexp::Str(x) => write!(f, "\"{}\"", x.escape_default()),
            Sexp::Ident(x) => write!(f, "{}", x.as_ref()),
            Sexp::Op(x) => write!(f, "{}", x.as_ref()),
            Sexp::List(xs) => write!(f, "({})", xs.iter().map(|x| format!("{x}")).join(" ")),
        }
    }
}
