use crate::{core::*, utils::unescape::*};
use anyhow::{anyhow, Ok};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use smartstring::alias::String;
use std::{hash::Hash, vec};

#[derive(Parser)]
#[grammar = "parser/core.pest"]
struct CoreParser;

type PResult<T> = Result<T, anyhow::Error>;

#[derive(Debug)]
enum Atom<'a> {
    Ident(&'a str),
    Op(&'a str),
    Literal(Literal),
}

#[derive(Debug)]
enum Sexp<'a> {
    SAtom(Atom<'a>),
    SExpr(Vec<Sexp<'a>>),
}

fn parse_ident(pair: Pair<Rule>) -> Option<String> {
    assert_eq!(pair.as_rule(), Rule::ident);
    if pair.as_str() == "_" {
        None
    } else {
        Some(pair.as_str().into())
    }
}

fn parse_integer(pair: Pair<Rule>) -> PResult<i64> {
    assert_eq!(pair.as_rule(), Rule::integer);
    Ok(pair.as_str().parse::<i64>()?)
}

fn parse_float(pair: Pair<Rule>) -> PResult<f64> {
    assert_eq!(pair.as_rule(), Rule::float);
    Ok(pair.as_str().parse::<f64>()?)
}

fn parse_string(pair: Pair<Rule>) -> PResult<String> {
    assert_eq!(pair.as_rule(), Rule::string);
    Ok(unescape(pair.as_str())?)
}

fn parse_raw_string(pair: Pair<Rule>) -> PResult<String> {
    assert_eq!(pair.as_rule(), Rule::raw_string);
    Ok(pair.as_str().into())
}

fn parse_bool(pair: Pair<Rule>) -> PResult<bool> {
    assert_eq!(pair.as_rule(), Rule::bool);
    Ok(pair.as_str().parse::<bool>()?)
}

fn parse_literal(pair: Pair<Rule>) -> PResult<Literal> {
    assert_eq!(pair.as_rule(), Rule::literal);
    let pair = pair.into_inner().next().unwrap();
    let r = match pair.as_rule() {
        Rule::float => Literal::Float(parse_float(pair)?),
        Rule::integer => Literal::Integer(parse_integer(pair)?),
        Rule::string => Literal::Str(parse_string(pair)?),
        Rule::raw_string => Literal::Str(parse_raw_string(pair)?),
        Rule::bool => Literal::Bool(parse_bool(pair)?),
        _ => unreachable!(),
    };
    Ok(r)
}

fn parse_atom(pair: Pair<Rule>) -> PResult<Atom> {
    assert_eq!(pair.as_rule(), Rule::atom);
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::literal => Atom::Literal(parse_literal(pair)?),
        Rule::ident => Atom::Ident(pair.as_str().into()),
        Rule::op => Atom::Op(pair.as_str().into()),
        _ => unreachable!(),
    })
}

fn parse_sexp(pair: Pair<Rule>) -> PResult<Sexp> {
    assert_eq!(pair.as_rule(), Rule::sexp);
    let mut vec = vec![];
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::atom => return Ok(Sexp::SAtom(parse_atom(pair)?)),
            Rule::sexp => vec.push(parse_sexp(pair)?),
            _ => unreachable!(),
        };
    }
    Ok(Sexp::SExpr(vec))
}

impl TryFrom<&Sexp<'_>> for TopBinding {
    type Error = anyhow::Error;
    fn try_from(value: &Sexp<'_>) -> Result<Self, Self::Error> {
        use Atom::*;
        use Sexp::*;
        match value {
            SExpr(xs) => match xs.as_slice() {
                [SAtom(Ident("type")), SAtom(Ident(x)), ty] => {
                    let ty: Type = ty.try_into()?;
                    Ok(TopBinding::Type((*x).into(), ty))
                }
                [SAtom(Ident("def")), SAtom(Ident(x)), expr] => {
                    let expr: Type = expr.try_into()?;
                    Ok(TopBinding::Type((*x).into(), expr))
                }
                _ => Err(anyhow!("unknown top binding s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown top binding s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<&Sexp<'_>> for Expr {
    type Error = anyhow::Error;
    fn try_from(value: &Sexp) -> Result<Self, Self::Error> {
        use Atom::*;
        use Sexp::*;
        match value {
            SAtom(Literal(x)) => Ok(Expr::Literal(Box::new(x.clone()))),
            SAtom(Op(x)) if OP_NAME.contains(x) => {
                Ok(Expr::Operator(Operator(OP_NAME.get(*x).unwrap())))
            }
            SAtom(Ident(x)) => Ok(Expr::Var((*x).into())),
            SExpr(xs) => match xs.as_slice() {
                [] => Ok(Expr::Unit),
                [SAtom(Ident("tuple")), xs @ ..] => {
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::Inj(None, exprs))
                }
                [SAtom(Ident("inj") | Ident("#")), SAtom(Ident(name)), xs @ ..] => {
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::Inj(Some((*name).into()), exprs))
                }
                [SAtom(Op("@")), f, xs @ ..] => {
                    let f: Expr = f.try_into()?;
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::App(f.pack(), exprs))
                }
                [x, SAtom(Op("::")), ty] => {
                    let x: Expr = x.try_into()?;
                    let ty: Type = ty.try_into()?;
                    Ok(Expr::Anno(x.pack(), ty.pack()))
                }
                [SAtom(Ident("if")), cond, tru, fls] => {
                    let cond: Expr = cond.try_into()?;
                    let tru: Expr = tru.try_into()?;
                    let fls: Expr = fls.try_into()?;
                    Ok(Expr::If(cond.pack(), tru.pack(), fls.pack()))
                }
                [SAtom(Ident("lambda") | Op("\\")), p @ SExpr(params), expr] => {
                    let mut names: Vec<(String, Option<TyRef>)> = vec![];
                    for param in params.as_slice() {
                        match param {
                            SAtom(Ident(name)) => names.push(((*name).into(), None)),
                            SExpr(xs) => {
                                let [SAtom(Ident(name)), ty] = xs.as_slice() else {
                                    return Err(anyhow!("not an annotated parameter: {:?}", param));
                                };
                                let ty: Type = ty.try_into()?;
                                names.push(((*name).into(), Some(ty.into())))
                            }
                            _ => return Err(anyhow!("unknown parameters s-expression: {:?}", p)),
                        }
                    }
                    let expr: Expr = expr.try_into()?;
                    Ok(Expr::Abs(names, expr.pack()))
                }
                [SAtom(Ident("let")), SAtom(Ident(x)), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Let((*x).into(), e1.pack(), e2.pack()))
                }
                [SAtom(Ident("try")), SAtom(Ident(x)), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Try((*x).into(), e1.pack(), e2.pack()))
                }
                [SAtom(Ident("resume")), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Resume(e1.pack(), e2.pack()))
                }
                [SAtom(Ident("raise")), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Raise(e1.pack(), e2.pack()))
                }
                [SAtom(Ident("proj")), e, SAtom(Literal(crate::core::Literal::Integer(x)))]
                | [e, SAtom(Ident(".")), SAtom(Literal(crate::core::Literal::Integer(x)))] => {
                    let e: Expr = e.try_into()?;
                    Ok(Expr::Proj(e.pack(), *x as isize))
                }
                _ => Err(anyhow!("unknown expr s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown expr s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<&Sexp<'_>> for Type {
    type Error = anyhow::Error;
    fn try_from(value: &Sexp) -> Result<Self, Self::Error> {
        use Atom::*;
        use Sexp::*;
        match value {
            SAtom(Ident("unit")) => Ok(Type::Unit),
            SAtom(Ident("bool")) => Ok(Type::Bool),
            SAtom(Ident("str")) => Ok(Type::Str),
            SAtom(Ident("int")) => Ok(Type::Integer),
            SAtom(Ident(name)) => Ok(Type::Named((*name).into())),
            SExpr(xs) => match xs.as_slice() {
                [SAtom(Ident("tuple")), xs @ ..] => {
                    let mut tys = vec![];
                    for sexp in xs {
                        tys.push(sexp.try_into()?)
                    }
                    Ok(Type::Tuple(tys))
                }
                [SAtom(Ident("ctor") | Op("#")), SAtom(Ident(name)), xs @ ..] => {
                    let mut tys = vec![];
                    for sexp in xs {
                        tys.push(sexp.try_into()?)
                    }
                    Ok(Type::Ctor((*name).into(), tys))
                }
                [SAtom(Ident("variant")), xs @ ..] => {
                    let mut fields = vec![];
                    for sexp in xs {
                        let SExpr(sexp) = sexp else {
                            return Err(anyhow!("not a valid field in a variant"))
                        };
                        let [SAtom(Ident(s)), ys @ ..] = sexp.as_slice() else {
                            return Err(anyhow!("not a valid field in a variant"))
                        };
                        let mut tys = vec![];
                        for sexp in ys {
                            tys.push(sexp.try_into()?)
                        }
                        fields.push(((*s).into(), tys));
                    }
                    Ok(Type::Variant(fields))
                }
                [SAtom(Ident("forall")), SExpr(params), ty] => {
                    let mut names: Vec<String> = vec![];
                    for x in params.as_slice() {
                        let SAtom(Ident(name)) = x else {
                            return Err(anyhow!("forall requires identifiers as its parameters"));
                        };
                        names.push((*name).into());
                    }
                    let ty: Type = ty.try_into()?;
                    Ok(Type::All(names, ty.pack()))
                }
                [SAtom(Ident("arrow")), SExpr(t1), t2] | [SExpr(t1), SAtom(Op("->")), t2] => {
                    let mut t1s = vec![];
                    for ty in t1 {
                        t1s.push(ty.try_into()?)
                    }
                    let t2: Type = t2.try_into()?;
                    Ok(Type::Arrow(t1s, t2.pack(), None))
                }
                [SAtom(Ident("arrow")), SExpr(t1), t2, t3]
                | [SExpr(t1), SAtom(Op("->")), t2, SAtom(Op("/")), t3] => {
                    let mut t1s = vec![];
                    for ty in t1 {
                        t1s.push(ty.try_into()?)
                    }
                    let t2: Type = t2.try_into()?;
                    let t3: Type = t3.try_into()?;
                    Ok(Type::Arrow(t1s, t2.pack(), Some(t3.pack())))
                }
                _ => Err(anyhow!("unknown type s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown type s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<Sexp<'_>> for Type {
    type Error = anyhow::Error;
    fn try_from(value: Sexp<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<Sexp<'_>> for Expr {
    type Error = anyhow::Error;
    fn try_from(value: Sexp<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<Sexp<'_>> for TopBinding {
    type Error = anyhow::Error;
    fn try_from(value: Sexp<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

fn parse_sexp_as_type(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::sexp);
    parse_sexp(pair)?.try_into()
}

fn parse_sexp_as_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::sexp);
    parse_sexp(pair)?.try_into()
}

fn parse_sexp_as_top_binding(pair: Pair<Rule>) -> PResult<TopBinding> {
    assert_eq!(pair.as_rule(), Rule::sexp);
    parse_sexp(pair)?.try_into()
}

fn parse(input: &str) -> PResult<Module> {
    let module = CoreParser::parse(Rule::module, input)?.next().unwrap();
    let mut tb = vec![];
    for sexp in module.into_inner() {
        if sexp.as_rule() == Rule::EOI {
            break;
        }
        tb.push(parse_sexp_as_top_binding(sexp)?);
    }
    Ok(Module(tb))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse() {
        let input = r###"
(type List (forall [a] (variant [Cons a] [Nil])))
(type Cont (forall [t1 t2] (# cont t1 t2)))
        "###;
        println!("{:?}", parse(input).unwrap())
    }
}
