use anyhow::Ok;

use super::*;
use crate::substs;

impl ContextGroup {
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
