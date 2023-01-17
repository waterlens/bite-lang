use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::{Captures, Regex};
use smartstring::alias::String;
use std::{fmt::*, rc::Rc};

type TyRef = Rc<Type>;
#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Int,
    Void,
    Func(TyRef, Vec<Type>),
    Ptr(TyRef),
    Raw(String),
}

type ExRef = Rc<Expr>;
#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Call(ExRef, Vec<Expr>),
    Raw(String),
}

type StRef = Rc<Stmt>;
#[derive(Debug, Clone, PartialEq, Eq)]
enum Stmt {
    If(ExRef, StRef, StRef),
    Return(ExRef),
    Block(Vec<Stmt>),
    Expr(ExRef),
    Decl(String, TyRef, Option<ExRef>),
    Switch(ExRef, Vec<(Option<ExRef>, StRef)>),
    Break,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Func {
    name: String,
    ret: Type,
    parameters: Vec<(Option<String>, Type)>,
    statements: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Source {
    functions: Vec<Func>,
}

static REGEX_ARG_INDEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"#([[:digit:]]+)").unwrap());
static REGEX_ARG_LENGTH: Lazy<Regex> = Lazy::new(|| Regex::new(r"#~").unwrap());
static REGEX_ARG_LIST: Lazy<Regex> = Lazy::new(|| Regex::new(r"#@").unwrap());

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Void => write!(f, "void"),
            Type::Func(r, a) => {
                write!(
                    f,
                    "{}(*)({})",
                    r.as_ref(),
                    a.iter().map(|x| x.to_string()).join(", ")
                )
            }
            Type::Ptr(t) => write!(f, "{t}*"),
            Type::Raw(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Call(x, a) => write!(f, "{x}({})", a.iter().map(|t| t.to_string()).join(", ")),
            Expr::Raw(s) => write!(f, "{s}"),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Stmt::If(cond, tru, fls) => write!(f, "if {cond} {tru} {fls}"),
            Stmt::Return(e) => write!(f, "return {e};"),
            Stmt::Block(stmts) => write!(f, "{}", stmts.iter().map(|x| x.to_string()).join(" ")),
            Stmt::Expr(e) => write!(f, "{e};"),
            Stmt::Decl(name, ty, e) => write!(
                f,
                "{ty} {name}{};",
                e.as_ref().map_or("".to_string(), |x| " = ".to_string()
                    + x.to_string().as_str())
            ),
            Stmt::Switch(e, cases) => write!(
                f,
                "switch ({e}){{{}}}",
                cases
                    .iter()
                    .map(|(x, y)| if let Some(expr) = x {
                        format!("case {expr}: {y}")
                    } else {
                        format!("default: {y}")
                    })
                    .join("")
            ),
            Stmt::Break => write!(f, "break;"),
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let Self {
            name,
            ret,
            parameters,
            statements,
        } = self;
        write!(
            f,
            "{ret} {name}({}){}",
            parameters
                .iter()
                .map(|(name, ty)| if let Some(name) = name {
                    format!("{ty} {name}")
                } else {
                    ty.to_string()
                })
                .join(", "),
            statements.as_ref().map_or(";".to_string(), |stmts| stmts
                .iter()
                .map(|x| x.to_string())
                .join(" "))
        )
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let prologue = "#include \"runtime.h\"\n";
        let epilogue = "";
        write!(
            f,
            "{prologue}{}{epilogue}",
            self.functions.iter().map(|f| f.to_string()).join("")
        )
    }
}

fn subst_placeholder<D: Display>(s: &str, xs: &[D]) -> String {
    let len = xs.len();
    let s1 = REGEX_ARG_LENGTH.replace_all(s, len.to_string());
    let s2 = REGEX_ARG_INDEX.replace_all(s1.as_ref(), |cap: &Captures| {
        let idx: usize = cap.get(1).unwrap().as_str().parse().unwrap();
        let expr = &xs[idx];
        format!("{expr}")
    });
    let expr_list: Lazy<String, _> = Lazy::new(|| {
        xs.iter()
            .enumerate()
            .map(|(idx, x)| {
                if idx == 0 {
                    x.to_string()
                } else {
                    format!(", {x}")
                }
            })
            .fold(String::new(), |s1, s2| s1 + s2)
    });
    let s3 = REGEX_ARG_LIST.replace_all(s2.as_ref(), (*expr_list).as_str());
    s3.into()
}

impl Expr {
    pub fn raw(s: &str, exprs: &[Expr]) -> Self {
        Expr::Raw(subst_placeholder(s, exprs))
    }

    pub fn value(s: &str) -> Self {
        Expr::Raw(s.into())
    }
}

impl Type {
    pub fn raw(s: &str, types: &[Type]) -> Self {
        Type::Raw(subst_placeholder(s, types))
    }
}

impl Func {
    pub fn create_declaration(
        name: &str,
        ret: &Type,
        parameters: &[(Option<String>, Type)],
    ) -> Self {
        Func {
            name: name.into(),
            ret: ret.clone(),
            parameters: parameters.to_vec(),
            statements: None,
        }
    }

    pub fn create_definition(
        name: &str,
        ret: &Type,
        parameters: &[(Option<String>, Type)],
        statements: &[Stmt],
    ) -> Self {
        Func {
            name: name.into(),
            ret: ret.clone(),
            parameters: parameters.to_vec(),
            statements: Some(statements.to_vec()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_raw_expr() {
        assert_eq!(
            Expr::raw(
                "#@, #0, #1, #2, #~",
                &[
                    Expr::value("true"),
                    Expr::value("1"),
                    Expr::value("2"),
                    Expr::value("3")
                ]
            ),
            Expr::Raw("true, 1, 2, 3, true, 1, 2, 4".into())
        );
    }

    #[test]
    fn test_function() {
        println!(
            "{}",
            Func::create_declaration(
                "f",
                &Type::Int,
                &[
                    (Some("x".into()), Type::Ptr(Type::Void.into())),
                    (None, Type::raw("int[]", &[]))
                ]
            )
        )
    }
}
