use crate::{core::*, utils::unescape::*};
use anyhow::Ok;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use smartstring::alias::String;
use std::{hash::Hash, vec};

#[derive(Parser)]
#[grammar = "parser/core.pest"]
struct CoreParser;

type PResult<T> = Result<T, anyhow::Error>;
type Sexp<'a> = crate::utils::sexp::Sexp<&'a str>;

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

fn parse_atom(pair: Pair<Rule>) -> PResult<Sexp> {
    assert_eq!(pair.as_rule(), Rule::atom);
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::literal => match parse_literal(pair)? {
            Literal::Str(x) => Sexp::Str(x),
            Literal::Integer(x) => Sexp::Integer(x),
            Literal::Float(x) => Sexp::Float(x),
            Literal::Bool(x) => Sexp::Bool(x),
        },
        Rule::ident => Sexp::Ident(pair.as_str().into()),
        Rule::op => Sexp::Op(pair.as_str().into()),
        _ => unreachable!(),
    })
}

fn parse_sexp(pair: Pair<Rule>) -> PResult<Sexp> {
    assert_eq!(pair.as_rule(), Rule::sexp);
    let mut vec = vec![];
    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::atom => return Ok(parse_atom(pair)?),
            Rule::sexp => vec.push(parse_sexp(pair)?),
            _ => unreachable!(),
        };
    }
    Ok(Sexp::List(vec))
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

pub fn parse_type(input: &str) -> PResult<Type> {
    let ty = CoreParser::parse(Rule::ty, input)?.next().unwrap();
    parse_sexp_as_type(ty.into_inner().next().unwrap())
}

pub fn parse_expr(input: &str) -> PResult<Expr> {
    let expr = CoreParser::parse(Rule::expr, input)?.next().unwrap();
    parse_sexp_as_expr(expr.into_inner().next().unwrap())
}

pub fn parse(input: &str) -> PResult<Module> {
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
(def _1 
    (try handle_id
        (\ 
            [(k :: (# cont int int))] 
            (\ (x) (resume k x)))
        (raise handle_id x)))
        "###;
        println!("{:?}", parse(input).unwrap())
    }
}
