use crate::utils::sexp::Sexp;

use super::*;
use itertools::Itertools;
use smartstring::alias::String;

pub type SexpWithStr<'a> = crate::utils::sexp::Sexp<&'a str>;
pub type SexpWithString = crate::utils::sexp::Sexp<String>;

impl TryFrom<&SexpWithStr<'_>> for Literal {
    type Error = anyhow::Error;

    fn try_from(value: &SexpWithStr<'_>) -> Result<Self, Self::Error> {
        use Literal::*;
        match value {
            SexpWithStr::Bool(x) => Ok(Bool(*x)),
            SexpWithStr::Integer(x) => Ok(Integer(*x)),
            SexpWithStr::Float(x) => Ok(Float(*x)),
            SexpWithStr::Str(x) => Ok(Str(x.clone())),
            SexpWithStr::Ident(_) | SexpWithStr::Op(_) | SexpWithStr::List(_) => {
                Err(anyhow!("not a literal"))
            }
        }
    }
}

impl TryFrom<&SexpWithStr<'_>> for TopBinding {
    type Error = anyhow::Error;
    fn try_from(value: &SexpWithStr<'_>) -> Result<Self, Self::Error> {
        use crate::utils::sexp::Sexp::*;
        match value {
            List(xs) => match xs.as_slice() {
                [Ident("type"), Ident(x), ty] => {
                    let ty: Type = ty.try_into()?;
                    Ok(TopBinding::Type((*x).into(), P(ty)))
                }
                [Ident("def"), Ident(x), Op("::"), ty, expr] => {
                    let expr: Expr = expr.try_into()?;
                    let ty: Type = ty.try_into()?;
                    Ok(TopBinding::Expr((*x).into(), P(ty), P(expr)))
                }
                _ => Err(anyhow!("unknown top binding s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown top binding s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<&SexpWithStr<'_>> for Expr {
    type Error = anyhow::Error;
    fn try_from(value: &SexpWithStr<'_>) -> Result<Self, Self::Error> {
        use crate::utils::sexp::Sexp::*;
        match value {
            Bool(_) | Integer(_) | Float(_) | Str(_) => {
                Ok(Expr::Literal(P(value.try_into().unwrap())))
            }
            Op(x) if OP_NAME.contains(x) => Ok(Expr::Operator(Operator(OP_NAME.get(*x).unwrap()))),
            Ident(x) => Ok(Expr::Var((*x).into())),
            List(xs) => match xs.as_slice() {
                [] => Ok(Expr::Unit),
                [Ident("tuple") | Op("$"), xs @ ..] => {
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::Inj(None, exprs.into()))
                }
                [Ident("inj") | Op("#"), Ident(name), xs @ ..] => {
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::Inj(Some((*name).into()), exprs.into()))
                }
                [Op("@"), f, xs @ ..] => {
                    let f: Expr = f.try_into()?;
                    let mut exprs = vec![];
                    for sexp in xs {
                        exprs.push(sexp.try_into()?)
                    }
                    Ok(Expr::App(P(f), exprs.into()))
                }
                [x, Op("::"), ty] => {
                    let x: Expr = x.try_into()?;
                    let ty: Type = ty.try_into()?;
                    Ok(Expr::Anno(P(x), P(ty)))
                }
                [Ident("if"), cond, tru, fls] => {
                    let cond: Expr = cond.try_into()?;
                    let tru: Expr = tru.try_into()?;
                    let fls: Expr = fls.try_into()?;
                    Ok(Expr::If(P(cond), P(tru), P(fls)))
                }
                [Ident("lambda") | Op("\\"), p @ List(params), expr] => {
                    let mut names: Vec<(String, Option<TyRef>)> = vec![];
                    for param in params.as_slice() {
                        match param {
                            Ident(name) => names.push(((*name).into(), None)),
                            List(xs) => {
                                let [Ident(name), Op("::"), ty] = xs.as_slice() else {
                                    return Err(anyhow!("not an annotated parameter: {:?}", param));
                                };
                                let ty: Type = ty.try_into()?;
                                names.push(((*name).into(), Some(ty.into())))
                            }
                            _ => return Err(anyhow!("unknown parameters s-expression: {:?}", p)),
                        }
                    }
                    let expr: Expr = expr.try_into()?;
                    Ok(Expr::Abs(names.into(), P(expr)))
                }
                [Ident("let"), Ident(x), Op("::"), ty, e1, e2] => {
                    let ty: Type = ty.try_into()?;
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Let((*x).into(), Some(P(ty)), P(e1), P(e2)))
                }
                [Ident("let"), Ident(x), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Let((*x).into(), None, P(e1), P(e2)))
                }
                [Ident("try"), Ident(x), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Try((*x).into(), P(e1), P(e2)))
                }
                [Ident("resume"), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Resume(P(e1), P(e2)))
                }
                [Ident("raise"), e1, e2] => {
                    let e1: Expr = e1.try_into()?;
                    let e2: Expr = e2.try_into()?;
                    Ok(Expr::Raise(P(e1), P(e2)))
                }
                [Ident("proj"), e, Integer(x)] | [e, Ident("."), Integer(x)] => {
                    let e: Expr = e.try_into()?;
                    Ok(Expr::Proj(P(e), (*x).try_into().unwrap()))
                }
                _ => Err(anyhow!("unknown expr s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown expr s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<&SexpWithStr<'_>> for Type {
    type Error = anyhow::Error;
    fn try_from(value: &SexpWithStr<'_>) -> Result<Self, Self::Error> {
        use crate::utils::sexp::Sexp::*;
        match value {
            Ident("unit") => Ok(Type::Unit),
            Ident("bool") => Ok(Type::Bool),
            Ident("str") => Ok(Type::Str),
            Ident("int") => Ok(Type::Integer),
            Ident(name) => Ok(Type::Named((*name).into())),
            List(xs) => match xs.as_slice() {
                [Ident("tuple"), xs @ ..] => {
                    let mut tys = vec![];
                    for sexp in xs {
                        tys.push(sexp.try_into()?)
                    }
                    Ok(Type::Tuple(tys.into()))
                }
                [Ident("ctor") | Op("#"), Ident(name), xs @ ..] => {
                    let mut tys = vec![];
                    for sexp in xs {
                        tys.push(sexp.try_into()?)
                    }
                    Ok(Type::Ctor((*name).into(), tys.into()))
                }
                [Ident("variant"), xs @ ..] => {
                    let mut fields = vec![];
                    for sexp in xs {
                        let List(sexp) = sexp else {
                            return Err(anyhow!("not a valid field in a variant"))
                        };
                        let [Ident(s), ys @ ..] = sexp.as_slice() else {
                            return Err(anyhow!("not a valid field in a variant"))
                        };
                        let mut tys = vec![];
                        for sexp in ys {
                            tys.push(sexp.try_into()?)
                        }
                        fields.push(((*s).into(), tys.into()));
                    }
                    Ok(Type::Variant(fields.into()))
                }
                [Ident("forall"), List(params), ty] => {
                    let mut names: Vec<String> = vec![];
                    for x in params.as_slice() {
                        let Ident(name) = x else {
                            return Err(anyhow!("forall requires identifiers as its parameters"));
                        };
                        names.push((*name).into());
                    }
                    let ty: Type = ty.try_into()?;
                    Ok(names
                        .into_iter()
                        .rfold(ty, |t, param| Type::All(param, P(t))))
                }
                [Ident("arrow"), List(t1), t2] | [List(t1), Op("->"), t2] => {
                    let mut t1s = vec![];
                    for ty in t1 {
                        t1s.push(ty.try_into()?)
                    }
                    let t2: Type = t2.try_into()?;
                    Ok(Type::Arrow(t1s.into(), P(t2), None))
                }
                [Ident("arrow"), List(t1), t2, List(t3)]
                | [List(t1), Op("->"), t2, Op("/"), List(t3)] => {
                    let mut t1s = vec![];
                    let mut t3s = vec![];
                    for ty in t1 {
                        t1s.push(ty.try_into()?)
                    }
                    for ty in t3 {
                        t3s.push(ty.try_into()?)
                    }
                    let t2: Type = t2.try_into()?;
                    Ok(Type::Arrow(t1s.into(), P(t2), Some(t3s.into())))
                }
                _ => Err(anyhow!("unknown type s-expression: {:?}", value)),
            },
            _ => Err(anyhow!("unknown type s-expression: {:?}", value)),
        }
    }
}

impl TryFrom<SexpWithStr<'_>> for Type {
    type Error = anyhow::Error;
    fn try_from(value: SexpWithStr<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<SexpWithStr<'_>> for Expr {
    type Error = anyhow::Error;
    fn try_from(value: SexpWithStr<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<SexpWithStr<'_>> for TopBinding {
    type Error = anyhow::Error;
    fn try_from(value: SexpWithStr<'_>) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl From<&Type> for SexpWithString {
    fn from(value: &Type) -> Self {
        use crate::utils::sexp::Sexp::*;
        match value {
            Type::Unit => Ident("unit".into()),
            Type::Str => Ident("str".into()),
            Type::Integer => Ident("int".into()),
            Type::Float => Ident("float".into()),
            Type::Bool => Ident("bool".into()),
            Type::Var(x) => Ident(format!("`{x}").into()),
            Type::Named(x) => Ident(x.clone()),
            Type::All(x, t) => {
                let mut params = vec![Ident(x.clone())];
                let mut t = t.as_ref();
                while let Type::All(y, t2) = t {
                    params.push(Ident(y.clone()));
                    t = t2;
                }
                List(vec![Ident("forall".into()), List(params), t.into()])
            }
            Type::Arrow(t1, t2, t3) => {
                if let Some(t3) = t3.as_ref() {
                    List(vec![
                        List(t1.iter().map(|x| x.into()).collect()),
                        Op("->".into()),
                        t2.as_ref().into(),
                        Op("/".into()),
                        List(t3.iter().map(|x| x.into()).collect()),
                    ])
                } else {
                    List(vec![
                        List(t1.iter().map(|x| x.into()).collect()),
                        Op("->".into()),
                        t2.as_ref().into(),
                    ])
                }
            }
            Type::Variant(xs) => {
                let mut v = vec![Ident("variant".into())];
                v.extend(xs.iter().map(|(s, tys)| {
                    let mut v = vec![Ident(s.clone())];
                    v.extend(tys.iter().map(|ty| ty.into()));
                    List(v)
                }));
                List(v)
            }
            Type::Tuple(xs) => {
                let mut v = vec![Ident("tuple".into())];
                v.extend(xs.iter().map(|ty| ty.into()));
                List(v)
            }
            Type::Ctor(x, xs) => {
                let mut v = vec![Op("ctor".into()), Ident(x.clone())];
                v.extend(xs.iter().map(|t| t.into()));
                List(v)
            }
        }
    }
}

impl From<&Expr> for SexpWithString {
    fn from(value: &Expr) -> Self {
        use crate::utils::sexp::Sexp::*;
        match value {
            Expr::Unit => List(vec![]),
            Expr::Anno(x, t) => List(vec![x.as_ref().into(), Op("::".into()), t.as_ref().into()]),
            Expr::Literal(x) => match x.as_ref() {
                Literal::Str(x) => Str(x.clone()),
                Literal::Integer(x) => Integer(*x),
                Literal::Float(x) => Float(*x),
                Literal::Bool(x) => Bool(*x),
            },
            Expr::Var(x) => Ident(x.clone()),
            Expr::Operator(x) => Op(x.0.into()),
            Expr::If(e1, e2, e3) => List(vec![
                Ident("if".into()),
                e1.as_ref().into(),
                e2.as_ref().into(),
                e3.as_ref().into(),
            ]),
            Expr::Abs(xs, e) => List(vec![
                Op("\\".into()),
                List(
                    xs.iter()
                        .map(|(s, ty)| {
                            if let Some(ty) = ty {
                                List(vec![Ident(s.clone()), Op("::".into()), ty.as_ref().into()])
                            } else {
                                Ident(s.clone())
                            }
                        })
                        .collect(),
                ),
                e.as_ref().into(),
            ]),
            Expr::App(f, xs) => {
                let mut v = vec![Op("@".into()), f.as_ref().into()];
                v.extend(xs.iter().map(|x| x.into()));
                List(v)
            }
            Expr::AppClosure(closure, xs) => {
                let mut v = vec![Op("apply-closure".into()), closure.as_ref().into()];
                v.extend(xs.iter().map(|x| x.into()));
                List(v)
            }
            Expr::AppDirectly(closure, xs) => {
                let mut v = vec![Op("apply-directly".into()), closure.as_ref().into()];
                v.extend(xs.iter().map(|x| x.into()));
                List(v)
            }
            Expr::Inj(Some(name), xs) => {
                let mut v = vec![Op("inj".into()), Ident(name.clone())];
                v.extend(xs.iter().map(|x| x.into()));
                List(v)
            }
            Expr::Inj(None, xs) => {
                let mut v = vec![Op("tuple".into())];
                v.extend(xs.iter().map(|x| x.into()));
                List(v)
            }
            Expr::Proj(e, n) => List(vec![
                e.as_ref().into(),
                Op(".".into()),
                Integer((*n).try_into().unwrap()),
            ]),
            Expr::Case(_, _) => unimplemented!(),
            Expr::Let(x, Some(t), e1, e2) => List(vec![
                Ident("let".into()),
                Ident(x.clone()),
                Op("::".into()),
                t.as_ref().into(),
                e1.as_ref().into(),
                e2.as_ref().into(),
            ]),
            Expr::Let(x, None, e1, e2) => List(vec![
                Ident("let".into()),
                Ident(x.clone()),
                e1.as_ref().into(),
                e2.as_ref().into(),
            ]),
            Expr::Try(x, e1, e2) => List(vec![
                Ident("try".into()),
                Ident(x.clone()),
                e1.as_ref().into(),
                e2.as_ref().into(),
            ]),
            Expr::Resume(k, e) => List(vec![
                Ident("resume".into()),
                k.as_ref().into(),
                e.as_ref().into(),
            ]),
            Expr::Raise(k, e) => List(vec![
                Ident("raise".into()),
                k.as_ref().into(),
                e.as_ref().into(),
            ]),
            Expr::TAbs(_, _) => unimplemented!(),
            Expr::TApp(_, _) => unimplemented!(),
        }
    }
}

impl From<&TopBinding> for SexpWithString {
    fn from(value: &TopBinding) -> Self {
        use crate::utils::sexp::Sexp::*;
        match value {
            TopBinding::Type(name, ty) => List(vec![
                Ident("type".into()),
                Ident(name.clone()),
                ty.as_ref().into(),
            ]),
            TopBinding::Expr(name, ty, expr) => List(vec![
                Ident("def".into()),
                Ident(name.clone()),
                Op("::".into()),
                ty.as_ref().into(),
                expr.as_ref().into(),
            ]),
        }
    }
}

impl From<&Module> for SexpWithString {
    fn from(value: &Module) -> Self {
        Sexp::List(value.0.iter().map(|x| x.into()).collect_vec())
    }
}

impl From<Type> for SexpWithString {
    fn from(value: Type) -> Self {
        (&value).into()
    }
}

impl From<Expr> for SexpWithString {
    fn from(value: Expr) -> Self {
        (&value).into()
    }
}

impl From<TopBinding> for SexpWithString {
    fn from(value: TopBinding) -> Self {
        (&value).into()
    }
}

impl From<Module> for SexpWithString {
    fn from(value: Module) -> Self {
        (&value).into()
    }
}
