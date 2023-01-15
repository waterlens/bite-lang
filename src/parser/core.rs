use crate::{core::*, utils::unescape::*};
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use smartstring::alias::String;
use std::{hash::Hash, vec};
use thiserror::Error;

#[derive(Parser)]
#[grammar = "parser/core.pest"]
struct CoreParser;

type PResult<T> = Result<T, anyhow::Error>;

#[derive(Debug, Error)]
#[error("wildcard is not allowed in this context")]
struct WildcardIsNotAllowed;

#[derive(Debug, Error)]
#[error("not an operator: {0}")]
struct NotAnOperator(String);

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

fn parse_ty_tuple(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_tuple);
    let mut tys = vec![];
    for pair in pair.into_inner() {
        let ty = parse_ty(pair)?;
        tys.push(ty);
    }
    Ok(Type::Tuple(tys))
}

fn parse_ty_ctor_impl(pair: Pair<Rule>) -> PResult<(String, Vec<Type>)> {
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    let name = parse_ident(pair).ok_or(WildcardIsNotAllowed)?;
    let mut tys = vec![];
    for pair in iter {
        let ty = parse_ty(pair)?;
        tys.push(ty);
    }
    Ok((name, tys))
}

fn parse_ty_ctor(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_ctor);
    let (name, tys) = parse_ty_ctor_impl(pair)?;
    Ok(Type::Ctor(name, tys))
}

fn parse_ty_variant(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_variant);
    let mut ctors = vec![];
    for pair in pair.into_inner() {
        let ctor = parse_ty_ctor_impl(pair)?;
        ctors.push(ctor)
    }
    Ok(Type::Variant(ctors))
}

fn parse_ty_atom(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_atom);
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    Ok(match pair.as_rule() {
        Rule::ty_ctor => parse_ty_ctor(pair)?,
        Rule::ident => {
            let s = parse_ident(pair).ok_or(WildcardIsNotAllowed)?;
            match s.as_str() {
                "int" => Type::Integer,
                "bool" => Type::Bool,
                "str" => Type::Str,
                _ => Type::Named(s),
            }
        }
        Rule::ty_tuple => parse_ty_tuple(pair)?,
        Rule::ty_variant => parse_ty_variant(pair)?,
        Rule::ty => parse_ty(pair)?,
        _ => unreachable!(),
    })
}

fn parse_tparam_list(pair: Pair<Rule>) -> PResult<Vec<String>> {
    assert_eq!(pair.as_rule(), Rule::tparam_list);
    let mut tparams = vec![];
    for pair in pair.into_inner() {
        tparams.push(parse_ident(pair).ok_or(WildcardIsNotAllowed)?);
    }
    Ok(tparams)
}

fn parse_ty_all(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_all);
    let mut iter = pair.into_inner();
    let tparam_list = parse_tparam_list(iter.next().unwrap())?;
    let ty = parse_ty(iter.next().unwrap())?;
    Ok(Type::All(tparam_list, ty.pack()))
}

fn parse_ty_arrow(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_arrow);
    let mut pair = pair.into_inner().rev();
    let mut init = pair.next().unwrap();
    let mut eff_name = if init.as_rule() == Rule::ident {
        let eff = parse_ident(init).ok_or(WildcardIsNotAllowed)?;
        init = pair.next().unwrap();
        Some(Type::Named(eff).pack())
    } else {
        None
    };
    let init_ty = parse_ty_atom(init)?;
    pair.try_fold(init_ty, |t2, pair| -> PResult<Type> {
        let t1 = parse_ty_atom(pair)?;
        Ok(Type::Arrow(
            t1.pack(),
            t2.pack(),
            if eff_name.is_some() {
                let eff = eff_name.clone();
                eff_name = None;
                eff
            } else {
                None
            },
        ))
    })
}

fn parse_ty(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty);
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    Ok(match pair.as_rule() {
        Rule::ty_all => parse_ty_all(pair)?,
        Rule::ty_arrow => parse_ty_arrow(pair)?,
        Rule::ty_atom => parse_ty_atom(pair)?,
        _ => unreachable!(),
    })
}

fn parse_op(pair: Pair<Rule>) -> PResult<Operator> {
    assert_eq!(pair.as_rule(), Rule::op);
    if OP_NAME.contains(pair.as_str()) {
        Ok(Operator {
            name: OP_NAME.get(pair.as_str()).unwrap(),
        })
    } else {
        Err(NotAnOperator(pair.as_str().into()).into())
    }
}

fn parse_expr_operator(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_operator);
    let op = parse_op(pair.into_inner().next().unwrap())?;
    Ok(Expr::Operator(op))
}

fn parse_expr_anno(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_anno);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let ty = parse_ty(iter.next().unwrap())?;
    Ok(Expr::Anno(expr.pack(), ty.pack()))
}

fn parse_expr_app(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_app);
    let mut iter = pair.into_inner();
    let f = parse_expr(iter.next().unwrap())?;
    let mut args = vec![];
    for pair in iter {
        let arg = parse_expr(pair)?;
        args.push(arg);
    }
    Ok(Expr::App(f.pack(), args))
}

fn parse_expr_inj(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_inj);
    let mut iter = pair.into_inner();
    let name = parse_ident(iter.next().unwrap()).ok_or(WildcardIsNotAllowed)?;
    let mut args = vec![];
    for pair in iter {
        let expr = parse_expr(pair)?;
        args.push(expr);
    }
    Ok(Expr::Inj(Some(name), args))
}

fn parse_expr_tuple(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_tuple);
    let mut fields = vec![];
    for pair in pair.into_inner() {
        let expr = parse_expr(pair)?;
        fields.push(expr);
    }
    Ok(Expr::Inj(None, fields))
}

fn parse_expr_atom(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_atom);
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    Ok(match pair.as_rule() {
        Rule::ident => Expr::Var(parse_ident(pair).ok_or(WildcardIsNotAllowed)?),
        Rule::literal => Expr::Literal(Box::new(parse_literal(pair)?)),
        Rule::expr => parse_expr(pair)?,
        Rule::expr_anno => parse_expr_anno(pair)?,
        Rule::expr_operator => parse_expr_operator(pair)?,
        Rule::expr_app => parse_expr_app(pair)?,
        Rule::expr_tuple => parse_expr_tuple(pair)?,
        Rule::expr_inj => parse_expr_inj(pair)?,
        rule @ _ => unreachable!("{:?}", rule),
    })
}

fn parse_expr_if(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_if);
    let mut iter = pair.into_inner();
    let cond = parse_expr(iter.next().unwrap())?;
    let t = parse_expr(iter.next().unwrap())?;
    let f = parse_expr(iter.next().unwrap())?;
    Ok(Expr::If(cond.pack(), t.pack(), f.pack()))
}

fn parse_param(pair: Pair<Rule>) -> PResult<(String, Type)> {
    assert_eq!(pair.as_rule(), Rule::param);
    let mut iter = pair.into_inner();
    let ident = parse_ident(iter.next().unwrap()).ok_or(WildcardIsNotAllowed)?;
    let ty = parse_ty(iter.next().unwrap())?;
    Ok((ident, ty))
}

fn parse_param_list(pair: Pair<Rule>) -> PResult<Vec<(String, Type)>> {
    assert_eq!(pair.as_rule(), Rule::param_list);
    let mut params = vec![];
    for pair in pair.into_inner() {
        let param = parse_param(pair)?;
        params.push(param);
    }
    Ok(params)
}

fn parse_expr_abs(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_abs);
    let mut iter = pair.into_inner();
    let pl = parse_param_list(iter.next().unwrap())?;
    let expr = parse_expr(iter.next().unwrap())?;
    Ok(Expr::Abs(pl, expr.pack()))
}

fn parse_case_arm(pair: Pair<Rule>) -> PResult<(String, Vec<Option<String>>, Expr)> {
    assert_eq!(pair.as_rule(), Rule::case_arm);
    let mut iter = pair.into_inner();
    let name = parse_ident(iter.next().unwrap()).ok_or(WildcardIsNotAllowed)?;
    let mut bds = vec![];
    let mut last = None;
    for pair in iter {
        if pair.as_rule() == Rule::ident {
            let var = parse_ident(pair);
            bds.push(var);
        } else {
            last = Some(parse_expr(pair)?);
        }
    }
    Ok((name, bds, last.unwrap()))
}

fn parse_expr_case(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_case);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let mut cases = vec![];
    for pair in iter {
        let case = parse_case_arm(pair)?;
        cases.push(case)
    }
    Ok(Expr::Case(expr.pack(), cases))
}

fn parse_expr_let(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_let);
    let mut iter = pair.into_inner();
    let name = parse_ident(iter.next().unwrap());
    let e1 = parse_expr(iter.next().unwrap())?;
    let e2 = parse_expr(iter.next().unwrap())?;
    Ok(Expr::Let(name, e1.pack(), e2.pack()))
}

fn parse_expr_try(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_let);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let label = parse_ident(iter.next().unwrap());
    let handler = parse_expr(iter.next().unwrap())?;
    Ok(Expr::Try(expr.pack(), label, handler.pack()))
}

fn parse_expr_resume(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_resume);
    let mut iter = pair.into_inner();
    let e1 = parse_expr(iter.next().unwrap())?;
    let e2 = parse_expr(iter.next().unwrap())?;
    Ok(Expr::Resume(e1.pack(), e2.pack()))
}

fn parse_expr_raise(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_raise);
    let mut iter = pair.into_inner();
    let e1 = parse_expr(iter.next().unwrap())?;
    let e2 = parse_expr(iter.next().unwrap())?;
    Ok(Expr::Raise(e1.pack(), e2.pack()))
}

fn parse_expr_tabs(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_tabs);
    let mut iter = pair.into_inner();
    let tpl = parse_tparam_list(iter.next().unwrap())?;
    let expr = parse_expr(iter.next().unwrap())?;
    Ok(Expr::TAbs(tpl, expr.pack()))
}

fn parse_expr_postfix(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_postfix);
    let mut iter = pair.into_inner();
    let init_expr = parse_expr_atom(iter.next().unwrap())?;
    iter.try_fold(init_expr, |expr, pair| -> PResult<Expr> {
        match pair.as_rule() {
            Rule::proj => Ok(Expr::Proj(expr.pack(), parse_integer(pair)? as isize)),
            Rule::tapp => Ok(Expr::TApp(expr.pack(), parse_tapp(pair)?)),
            _ => unreachable!(),
        }
    })
}

fn parse_proj(pair: Pair<Rule>) -> PResult<i64> {
    assert_eq!(pair.as_rule(), Rule::proj);
    parse_integer(pair.into_inner().next().unwrap())
}

fn parse_tapp(pair: Pair<Rule>) -> PResult<Vec<Type>> {
    assert_eq!(pair.as_rule(), Rule::tapp);
    let mut types = vec![];
    for pair in pair.into_inner() {
        types.push(parse_ty(pair)?)
    }
    Ok(types)
}

fn parse_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr);
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    Ok(match pair.as_rule() {
        Rule::expr_if => parse_expr_if(pair)?,
        Rule::expr_abs => parse_expr_abs(pair)?,
        Rule::expr_case => parse_expr_case(pair)?,
        Rule::expr_let => parse_expr_operator(pair)?,
        Rule::expr_try => parse_expr_try(pair)?,
        Rule::expr_resume => parse_expr_resume(pair)?,
        Rule::expr_raise => parse_expr_raise(pair)?,
        Rule::expr_tabs => parse_expr_tabs(pair)?,
        Rule::expr_postfix => parse_expr_postfix(pair)?,
        Rule::expr_atom => parse_expr_atom(pair)?,
        rule @ _ => unreachable!("{:?}", rule),
    })
}

fn parse_type_binding(pair: Pair<Rule>) -> PResult<TopBinding> {
    assert_eq!(pair.as_rule(), Rule::type_binding);
    let mut iter = pair.into_inner();
    let name = parse_ident(iter.next().unwrap()).ok_or(WildcardIsNotAllowed)?;
    let ty = parse_ty(iter.next().unwrap())?;
    Ok(TopBinding::Type(name, ty))
}

fn parse_expr_binding(pair: Pair<Rule>) -> PResult<TopBinding> {
    assert_eq!(pair.as_rule(), Rule::expr_binding);
    let mut iter = pair.into_inner();
    let name = parse_ident(iter.next().unwrap());
    let expr = parse_expr(iter.next().unwrap())?;
    Ok(TopBinding::Expr(name, expr))
}

fn parse_top_binding(pair: Pair<Rule>) -> PResult<TopBinding> {
    assert_eq!(pair.as_rule(), Rule::top_binding);
    let mut iter = pair.into_inner();
    let pair = iter.next().unwrap();
    match pair.as_rule() {
        Rule::type_binding => parse_type_binding(pair),
        Rule::expr_binding => parse_expr_binding(pair),
        _ => unreachable!(),
    }
}

fn parse_module(pair: Pair<Rule>) -> PResult<Module> {
    assert_eq!(pair.as_rule(), Rule::module);
    let mut bindings = vec![];
    for pair in pair.into_inner() {
        if pair.as_rule() == Rule::EOI {
            break;
        }
        let binding = parse_top_binding(pair)?;
        bindings.push(binding);
    }
    Ok(Module(bindings))
}

fn parse(input: &str) -> PResult<Module> {
    let module = CoreParser::parse(Rule::module, input)?.next().unwrap();
    parse_module(module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        parse(
            r###"
type a = <A(a), B(b)>;
let _ = ();
"###,
        )
        .unwrap();
    }
}
