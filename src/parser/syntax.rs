use crate::{syntax::*, utils::unescape::*};
use anyhow::Ok;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use smartstring::alias::String;
use std::{collections::HashMap, hash::Hash, vec};
use thiserror::Error;

#[derive(Parser)]
#[grammar = "parser/syntax.pest"]
struct LangParser;

type PResult<T> = Result<T, anyhow::Error>;

fn parse_ident(pair: Pair<Rule>) -> String {
    assert_eq!(pair.as_rule(), Rule::ident);
    pair.as_str().into()
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

fn parse_primary_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::primary_expr);
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::literal => Expr::Literal(parse_literal(pair)?),
        Rule::ident => Expr::Variable(parse_ident(pair)),
        Rule::grouped_expr => parse_grouped_expr(pair)?,
        Rule::tuple_expr => parse_tuple_expr(pair)?,
        Rule::expr_with_block => parse_expr_with_block(pair)?,
        Rule::attribute_item => Expr::Attr(parse_attribute_item(pair)?),
        _ => unreachable!(),
    })
}

fn parse_grouped_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::grouped_expr);
    let pair = pair.into_inner().next().unwrap();
    parse_expr(pair)
}

fn parse_tuple_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::tuple_expr);
    let mut elem = vec![];
    for pair in pair.into_inner() {
        elem.push(parse_expr(pair)?);
    }
    Ok(Expr::Tuple(elem))
}

fn parse_block_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::block_expr);
    let mut stmt = vec![];
    for pair in pair.into_inner() {
        stmt.push(parse_stmt(pair)?);
    }
    Ok(Expr::Block(stmt))
}

fn parse_postfix_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::postfix_expr);
    let mut iter = pair.into_inner();
    let init_expr = parse_primary_expr(iter.next().unwrap())?;
    iter.try_fold(init_expr, |expr, pair| -> PResult<Expr> {
        match pair.as_rule() {
            Rule::field_call => {
                let (ident, args) = parse_field_call(pair)?;
                Ok(Expr::FieldCall(Box::new(expr), ident, args))
            }
            Rule::call => {
                let args = parse_call(pair)?;
                Ok(Expr::Call(Box::new(expr), args))
            }
            _ => unreachable!(),
        }
    })
}

fn parse_field_call(pair: Pair<Rule>) -> PResult<(String, Vec<Expr>)> {
    assert_eq!(pair.as_rule(), Rule::field_call);
    let mut iter = pair.into_inner();
    let ident = parse_ident(iter.next().unwrap());
    let args = parse_argument_list(iter.next().unwrap())?;
    Ok((ident, args))
}

fn parse_call(pair: Pair<Rule>) -> PResult<Vec<Expr>> {
    assert_eq!(pair.as_rule(), Rule::call);
    let mut iter = pair.into_inner();
    let args = parse_argument_list(iter.next().unwrap())?;
    Ok(args)
}

fn parse_prefix_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::prefix_expr);
    let mut iter = pair.into_inner().rev();
    let init_expr = parse_postfix_expr(iter.next().unwrap())?;
    Ok(iter.fold(init_expr, |expr, pair| {
        let op = parse_op(pair);
        Expr::Unary(op, Box::new(expr))
    }))
}

fn parse_mul_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::mul_expr);
    let mut iter = pair.into_inner();
    let mut lhs = parse_prefix_expr(iter.next().unwrap())?;
    while let Some(op) = iter.next() {
        let op = parse_op(op);
        let rhs = parse_prefix_expr(iter.next().unwrap())?;
        lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

fn parse_add_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::add_expr);
    let mut iter = pair.into_inner();
    let mut lhs = parse_mul_expr(iter.next().unwrap())?;
    while let Some(op) = iter.next() {
        let op = parse_op(op);
        let rhs = parse_mul_expr(iter.next().unwrap())?;
        lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

fn parse_compare_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::compare_expr);
    let mut iter = pair.into_inner();
    let mut lhs = parse_add_expr(iter.next().unwrap())?;
    while let Some(op) = iter.next() {
        let op = parse_op(op);
        let rhs = parse_add_expr(iter.next().unwrap())?;
        lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

fn parse_op(pair: Pair<Rule>) -> Operator {
    assert!(matches!(
        pair.as_rule(),
        Rule::prefix_op | Rule::mul_op | Rule::add_op | Rule::compare_op
    ));
    use crate::core::*;
    Operator {
        name: OP_NAME.get(pair.as_str()).unwrap(),
    }
}

fn parse_expr_with_block(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr_with_block);
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::block_expr => parse_block_expr(pair)?,
        Rule::if_expr => parse_if_expr(pair)?,
        Rule::match_expr => parse_if_expr(pair)?,
        Rule::try_expr => parse_try_expr(pair)?,
        _ => unreachable!(),
    })
}

fn parse_if_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::if_expr);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let tbr = parse_block_expr(iter.next().unwrap())?;
    let fbr = if let Some(pair) = iter.next() {
        Some(Box::new(match pair.as_rule() {
            Rule::block_expr => parse_block_expr(pair)?,
            Rule::if_expr => parse_if_expr(pair)?,
            _ => unreachable!(),
        }))
    } else {
        None
    };
    Ok(Expr::If(Box::new(expr), Box::new(tbr), fbr))
}

fn parse_match_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::match_expr);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let arms = parse_arm_list(iter.next().unwrap())?;
    Ok(Expr::Match(Box::new(expr), arms))
}

fn parse_try_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::try_expr);
    assert_eq!(pair.as_rule(), Rule::match_expr);
    let mut iter = pair.into_inner();
    let expr = parse_expr(iter.next().unwrap())?;
    let arms = parse_arm_list(iter.next().unwrap())?;
    Ok(Expr::Try(Box::new(expr), arms))
}

fn parse_arm_with_block(pair: Pair<Rule>) -> PResult<(Pattern, Expr)> {
    assert_eq!(pair.as_rule(), Rule::arm_with_block);
    let mut iter = pair.into_inner();
    let pattern = parse_pattern(iter.next().unwrap())?;
    let expr = parse_expr_with_block(iter.next().unwrap())?;
    Ok((pattern, expr))
}

fn parse_arm_without_block(pair: Pair<Rule>) -> PResult<(Pattern, Expr)> {
    assert_eq!(pair.as_rule(), Rule::arm_without_block);
    let mut iter = pair.into_inner();
    let pattern = parse_pattern(iter.next().unwrap())?;
    let expr = parse_expr(iter.next().unwrap())?;
    Ok((pattern, expr))
}

fn parse_arm_list(pair: Pair<Rule>) -> PResult<Vec<(Pattern, Expr)>> {
    assert_eq!(pair.as_rule(), Rule::arm_list);
    let mut arms = vec![];
    let mut iter = pair.into_inner();
    while let Some(pair) = iter.next() {
        let r = match pair.as_rule() {
            Rule::arm_with_block => parse_arm_with_block(pair)?,
            Rule::arm_without_block => parse_arm_without_block(pair)?,
            Rule::pattern => {
                let pat = parse_pattern(pair)?;
                let pair = iter.next().unwrap();
                let expr = parse_expr(pair)?;
                (pat, expr)
            }
            _ => unreachable!(),
        };
        arms.push(r);
    }
    Ok(arms)
}

fn parse_pattern(pair: Pair<Rule>) -> PResult<Pattern> {
    assert_eq!(pair.as_rule(), Rule::pattern);
    let pair = pair.into_inner().next().unwrap();
    Ok(match pair.as_rule() {
        Rule::wildcard_pattern => parse_wildcard_pattern(pair)?,
        Rule::ctor_pattern => parse_ctor_pattern(pair)?,
        Rule::ident_pattern => parse_ident_pattern(pair)?,
        Rule::tuple_pattern => parse_tuple_pattern(pair)?,
        _ => unreachable!(),
    })
}

fn parse_wildcard_pattern(pair: Pair<Rule>) -> PResult<Pattern> {
    assert_eq!(pair.as_rule(), Rule::wildcard_pattern);
    Ok(Pattern::Wildcard)
}

fn parse_ident_pattern(pair: Pair<Rule>) -> PResult<Pattern> {
    assert_eq!(pair.as_rule(), Rule::ident_pattern);
    Ok(Pattern::Identifier(pair.as_str().into()))
}

fn parse_ctor_pattern(pair: Pair<Rule>) -> PResult<Pattern> {
    assert_eq!(pair.as_rule(), Rule::ctor_pattern);
    let mut iter = pair.into_inner();
    let ident = parse_ident(iter.next().unwrap());
    let pat = parse_tuple_pattern_impl(iter.next().unwrap())?;
    Ok(Pattern::Ctor(Some(ident), pat))
}

fn parse_tuple_pattern(pair: Pair<Rule>) -> PResult<Pattern> {
    assert_eq!(pair.as_rule(), Rule::tuple_pattern);
    let pat = parse_tuple_pattern_impl(pair)?;
    Ok(Pattern::Ctor(None, pat))
}

fn parse_tuple_pattern_impl(pair: Pair<Rule>) -> PResult<Vec<Pattern>> {
    assert_eq!(pair.as_rule(), Rule::tuple_pattern);
    let mut pat = vec![];
    for pair in pair.into_inner() {
        pat.push(parse_pattern(pair)?)
    }
    Ok(pat)
}

fn parse_resume_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::resume_expr);
    let pair = pair.into_inner().next().unwrap();
    let expr = parse_expr(pair)?;
    Ok(Expr::Resume(Box::new(expr)))
}

fn parse_raise_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::raise_expr);
    let pair = pair.into_inner().next().unwrap();
    let expr = parse_expr(pair)?;
    Ok(Expr::Raise(Box::new(expr)))
}

fn parse_expr(pair: Pair<Rule>) -> PResult<Expr> {
    assert_eq!(pair.as_rule(), Rule::expr);
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::expr_with_block => parse_expr_with_block(pair),
        Rule::resume_expr => parse_resume_expr(pair),
        Rule::raise_expr => parse_raise_expr(pair),
        Rule::compare_expr => parse_compare_expr(pair),
        _ => unreachable!(),
    }
}

fn parse_let_stmt(pair: Pair<Rule>) -> PResult<Stmt> {
    assert_eq!(pair.as_rule(), Rule::let_stmt);
    let mut iter = pair.into_inner();
    let pat = parse_pattern(iter.next().unwrap())?;
    let init_expr = if let Some(pair) = iter.next() {
        Some(Box::new(parse_expr(pair)?))
    } else {
        None
    };
    Ok(Stmt::Let {
        name: pat,
        ty: None,
        init: init_expr,
    })
}

fn parse_expr_stmt(pair: Pair<Rule>) -> PResult<Stmt> {
    assert_eq!(pair.as_rule(), Rule::expr_stmt);
    let pair = pair.into_inner().next().unwrap();
    Ok(Stmt::Expr(match pair.as_rule() {
        Rule::expr => parse_expr(pair)?,
        Rule::expr_with_block => parse_expr_with_block(pair)?,
        _ => unreachable!(),
    }))
}

fn parse_stmt(pair: Pair<Rule>) -> PResult<Stmt> {
    assert_eq!(pair.as_rule(), Rule::expr_stmt);
    todo!()
}

fn parse_stmts(pair: Pair<Rule>) -> PResult<Vec<Stmt>> {
    assert_eq!(pair.as_rule(), Rule::expr_stmt);
    todo!()
}

fn parse_ty(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty);
    parse_ty_arrow(pair.into_inner().next().unwrap())
}

fn parse_ty_var(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_var);
    let ident = pair.into_inner().next().unwrap();
    Ok(Type::Quoted(parse_ident(ident)))
}

fn parse_ty_prod(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_prod);
    let mut tys = vec![];
    for pair in pair.into_inner() {
        tys.push(parse_ty(pair)?);
    }
    Ok(Type::Tuple(tys))
}

fn parse_ty_atom(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_atom);
    let pair = pair.into_inner().next().unwrap();
    let ty = match pair.as_rule() {
        Rule::ident => Type::Named(parse_ident(pair)),
        Rule::ty_var => parse_ty_var(pair)?,
        Rule::ty_prod => parse_ty_prod(pair)?,
        _ => unreachable!(),
    };
    Ok(ty)
}

fn parse_ty_app(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_app);
    let mut pairs = pair.into_inner();
    let init = pairs.next().unwrap();
    let ty = parse_ty_atom(init)?;

    Ok(pairs.try_fold(ty, |t1, pair| -> PResult<Type> {
        let t2 = parse_ty_atom(pair)?;
        Ok(Type::App(Box::new(t1), Box::new(t2)))
    })?)
}

fn parse_ty_arrow(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::ty_arrow);
    let mut pairs = pair.into_inner().rev();
    let init = pairs.next().unwrap();
    let ty = parse_ty_app(init)?;

    Ok(pairs.try_fold(ty, |t2, pair| -> PResult<Type> {
        let t1 = parse_ty_atom(pair)?;
        Ok(Type::Arrow(Box::new(t1), Box::new(t2)))
    })?)
}

fn parse_argument_list(pair: Pair<Rule>) -> PResult<Vec<Expr>> {
    assert_eq!(pair.as_rule(), Rule::argument_list);
    let mut exprs = vec![];
    for pair in pair.into_inner() {
        exprs.push(parse_expr(pair)?);
    }
    Ok(exprs)
}

fn parse_parameter_list(pair: Pair<Rule>) -> PResult<Vec<Parameter>> {
    assert_eq!(pair.as_rule(), Rule::parameter_list);
    let mut params = vec![];
    for pair in pair.into_inner() {
        params.push(parse_parameter(pair)?);
    }
    Ok(params)
}

fn parse_parameter(pair: Pair<Rule>) -> PResult<Parameter> {
    assert_eq!(pair.as_rule(), Rule::parameter);
    let mut iter = pair.into_inner();
    let pat = parse_pattern(iter.next().unwrap())?;
    let ty = parse_ty(iter.next().unwrap())?;
    Ok(Parameter(Box::new(pat), Box::new(ty)))
}

fn parse_fn_def(pair: Pair<Rule>) -> PResult<FnDef> {
    assert_eq!(pair.as_rule(), Rule::fn_def);
    let mut iter = pair.into_inner();
    let attr = if iter.peek().unwrap().as_rule() == Rule::attribute_item {
        Some(parse_attribute_item(iter.next().unwrap())?)
    } else {
        None
    };
    let ident = parse_ident(iter.next().unwrap());
    let params = parse_parameter_list(iter.next().unwrap())?;
    let anno = parse_annotation_opt(iter.next().unwrap())?;
    let stmts = if let Some(pair) = iter.next() {
        parse_stmts(pair)?
    } else {
        vec![]
    };
    Ok(FnDef {
        name: ident,
        parameter: params,
        annotation: anno,
        body: stmts,
        attr,
    })
}

fn parse_eff_def(pair: Pair<Rule>) -> PResult<EffDef> {
    assert_eq!(pair.as_rule(), Rule::eff_def);
    let mut iter = pair.into_inner();
    let attr = if iter.peek().unwrap().as_rule() == Rule::attribute_item {
        Some(parse_attribute_item(iter.next().unwrap())?)
    } else {
        None
    };
    let ident = parse_ident(iter.next().unwrap());
    let params = parse_parameter_list(iter.next().unwrap())?;
    let anno = parse_annotation_opt(iter.next().unwrap())?;
    Ok(EffDef {
        name: ident,
        parameter: params,
        annotation: anno,
        attr,
    })
}

fn parse_annotation(pair: Pair<Rule>) -> PResult<Type> {
    assert_eq!(pair.as_rule(), Rule::annotation);
    let pair = pair.into_inner().next().unwrap();
    parse_ty(pair)
}

fn parse_annotation_opt(pair: Pair<Rule>) -> PResult<Option<Type>> {
    assert_eq!(pair.as_rule(), Rule::annotation_opt);
    let r = if let Some(x) = pair.into_inner().peek() {
        Some(parse_annotation(x)?)
    } else {
        None
    };
    Ok(r)
}

#[derive(Debug, Error)]
#[error("duplicate attribute: {0}")]
struct DuplicateAttribute(String);

fn parse_attribute(pair: Pair<Rule>) -> PResult<(String, Vec<Expr>)> {
    assert_eq!(pair.as_rule(), Rule::attribute);
    let mut iter = pair.into_inner();
    let ident = parse_ident(iter.next().unwrap());
    let args = if let Some(pair) = iter.next() {
        parse_argument_list(pair)?
    } else {
        vec![]
    };
    Ok((ident, args))
}

fn parse_attribute_item(pair: Pair<Rule>) -> PResult<Attribute> {
    assert_eq!(pair.as_rule(), Rule::attribute_item);
    let mut attrs = HashMap::new();
    for pair in pair.into_inner() {
        let (k, v) = parse_attribute(pair)?;
        if attrs.contains_key(&k) {
            return Err(DuplicateAttribute(k.clone()).into());
        }
        attrs.insert(k, v);
    }
    Ok(Attribute(attrs))
}

fn parse_module(pair: Pair<Rule>) -> PResult<Module> {
    assert_eq!(pair.as_rule(), Rule::module);
    let mut fn_def = vec![];
    let mut eff_def = vec![];
    for pair in pair.into_inner() {
        if pair.as_rule() == Rule::EOI {
            break;
        }
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::fn_def => fn_def.push(parse_fn_def(pair)?),
            Rule::eff_def => eff_def.push(parse_eff_def(pair)?),
            _ => unreachable!(),
        }
    }
    Ok(Module {
        fn_def,
        eff_def,
        type_def: vec![],
    })
}

fn parse(input: &str) -> PResult<Module> {
    let module = LangParser::parse(Rule::module, input)?.next().unwrap();
    parse_module(module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        parse(
            r###"
eff yield(elem: 'a): d;
#[a(expr)]
fn test() {
}

"###,
        )
        .unwrap();
    }
}
