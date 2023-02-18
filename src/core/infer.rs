use super::*;
use crate::substs;
use anyhow::Ok;
use itertools::Itertools;

impl Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Str(_) => Type::Str,
            Literal::Integer(_) => Type::Integer,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
        }
    }
}

impl ContextGroup {
    fn simple_infer(&self, expr: &Expr) -> Result<Type, anyhow::Error> {
        use Type::*;
        match expr {
            Expr::Unit => Ok(Unit),
            Expr::Anno(_e, ty) => Ok(ty.as_ref().clone()),
            Expr::Literal(lit) => Ok(lit.clone().and_then(|x| x.get_type())),
            Expr::Var(x) => {
                if let Some(ty) = self.lookup_var_ty(x.as_str()) {
                    if let Some(ty) = ty {
                        Ok(ty.as_ref().clone())
                    } else {
                        Err(anyhow!(
                            "variable {x} is not annotated and can't be infered"
                        ))
                    }
                } else {
                    Err(anyhow!("variable {x} is not defined yet"))
                }
            }
            Expr::Operator(Operator(name)) => {
                let ty = self.lookup_operator_ty(name).unwrap();
                Ok(ty.as_ref().clone())
            }
            Expr::If(cond, br1, br2) => {
                let t1 = self.simple_infer(cond.as_ref())?;
                let t2 = self.simple_infer(br1.as_ref())?;
                let t3 = self.simple_infer(br2.as_ref())?;

                if !self.type_eq(&Type::Bool, &t1) {
                    return Err(anyhow!("if requires its condition to be a boolean value"));
                }

                if !self.type_eq(&t2, &t3) {
                    return Err(anyhow!("incompatible types: {t2:?} {t3:?}"));
                }
                Ok(t2)
            }
            Expr::Abs(xs, e) => {
                let t = self.simple_infer(e.as_ref())?;
                let ts = xs
                    .iter()
                    .map(|(x, t)| {
                        t.as_ref()
                            .unwrap_or_else(|| panic!("parameter {x} should be annotated"))
                            .clone()
                            .into_inner()
                    })
                    .collect_vec();

                Ok(Type::Arrow(ts.into(), P(t), None))
            }
            Expr::App(f, xs) => {
                let tf = self.simple_infer(f)?;
                let mut txs = vec![];
                for x in xs {
                    txs.push(self.simple_infer(x)?);
                }
                let Type::Arrow(xs, t, _) = tf else { return Err(anyhow!("can't apply this")) };
                if !self.types_eq(&xs, &txs) {
                    return Err(anyhow!("incompatible arguments and call sites"));
                }
                Ok(t.as_ref().clone())
            }
            Expr::AppClosure(_, _) => todo!(),
            Expr::AppDirectly(_, _) => todo!(),
            Expr::Inj(name, xs) => {
                let mut ts = vec![];
                for x in xs {
                    ts.push(self.simple_infer(x)?);
                }

                match name {
                    Some(x) if x == "ref" => {
                        assert_eq!(xs.len(), 1);
                        Ok(Type::Ctor(Some(x.clone()), ts.into()))
                    }
                    Some(x) if x == "closure" => Ok(Type::Ctor(Some(x.clone()), ts.into())),
                    None => Ok(Type::Ctor(None, ts.into())),
                    _ => Err(anyhow!("unknown injection: {name:?}")),
                }
            }
            Expr::Proj(e, idx) => {
                let t = self.simple_infer(e.as_ref())?;
                let Type::Ctor(name, xs) = t else { return Err(anyhow!("can't project this")) };
                match name {
                    Some(x) if x == "closure" => (),
                    None => (),
                    _ => return Err(anyhow!("unknown ctor {name:?}"))
                }
                let idx = *idx as usize;
                if idx as usize >= xs.len() {
                    return Err(anyhow!(""))
                }
                Ok(xs[idx].clone())
            },
            Expr::Case(_, _) => todo!(),
            Expr::Let(x, t, _, e2) => {
                let ctx = self.insert_var_ty(x, t.clone());
                ctx.simple_infer(e2)
            },
            Expr::Try(_, _, _) => todo!(),
            Expr::Resume(_, _) => todo!(),
            Expr::Raise(_, _) => todo!(),
            Expr::TAbs(_, _) => todo!(),
            Expr::TApp(_, _) => todo!(),
        }
    }
    // do not generalize or instantiate things now
    fn infer(&self, expr: &Expr) -> Result<(Vec<Subst>, Type), anyhow::Error> {
        use Type::*;
        match expr {
            Expr::Unit => Ok((substs![], Unit)),
            Expr::Anno(_e, ty) => Ok((substs![], ty.as_ref().clone())),
            Expr::Literal(lit) => match lit.as_ref() {
                Literal::Str(_) => Ok((substs![], Str)),
                Literal::Integer(_) => Ok((substs![], Integer)),
                Literal::Float(_) => Ok((substs![], Float)),
                Literal::Bool(_) => Ok((substs![], Bool)),
            },
            Expr::Var(x) => {
                if let Some(ty) = self.lookup_var_ty(x.as_str()) {
                    if let Some(ty) = ty {
                        Ok((substs![], ty.clone().into_inner()))
                    } else {
                        Err(anyhow!(
                            "variable {x} is not annotated and can't be infered"
                        ))
                    }
                } else {
                    Err(anyhow!("variable {x} is not defined yet"))
                }
            }
            Expr::Operator(Operator(name)) => {
                let ty = self.lookup_operator_ty(name).unwrap();
                Ok((substs![], ty.clone().into_inner()))
            }
            Expr::If(_, _, _) => todo!(),
            Expr::Abs(_, _) => todo!(),
            Expr::App(_, _) => todo!(),
            Expr::AppClosure(_, _) => todo!(),
            Expr::AppDirectly(_, _) => todo!(),
            Expr::Inj(_, _) => todo!(),
            Expr::Proj(_, _) => todo!(),
            Expr::Case(_, _) => todo!(),
            Expr::Let(_, _, _, _) => todo!(),
            Expr::Try(_, _, _) => todo!(),
            Expr::Resume(_, _) => todo!(),
            Expr::Raise(_, _) => todo!(),
            Expr::TAbs(_, _) => todo!(),
            Expr::TApp(_, _) => todo!(),
        }
    }
}

impl Expr {
    fn type_propagation(self) -> Self {
        let mut work_list = vec![];
        let mut expr = self;
        let mut ctx = Context::new();
        while let Expr::Let(x, t, e1, e2) = expr {
            if t.is_some() {
                ctx = ctx.insert(x.clone(), t.clone());
                work_list.push((x, t, e1))
            } else {
                if let Expr::Var(name) = e1.as_ref() {
                    let Some(t) = ctx.lookup(name.as_str()) else { panic!("unknown variable {name}") };
                    let t = t.clone();
                    ctx = ctx.insert(x.clone(), t.clone());
                    work_list.push((x, t, e1))
                } else {
                    work_list.push((x, t, e1))
                }
            }
            expr = e2.into_inner()
        }
        expr.fold_bindings(work_list)
    }
}

impl Module {
    pub fn type_propagation(self) -> Self {
        Module(
            self.0
                .into_iter()
                .map(|tb| match tb {
                    TopBinding::Type(_, _) => tb,
                    TopBinding::Expr(x, t, e) => {
                        TopBinding::Expr(x, t, e.map(|x| x.type_propagation()))
                    }
                })
                .collect(),
        )
    }
}
