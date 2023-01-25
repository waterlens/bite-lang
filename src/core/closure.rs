use itertools::Itertools;

use super::*;

impl Module {
    pub fn closure_conversion(self) -> Self {
        let mut work_list = vec![];
        let mut ctx = ContextGroup::new();
        let fresh1 = RefCell::new(0);
        let fresh2 = RefCell::new(0);
        let fresh3 = RefCell::new(0);
        for binding in self.0.iter() {
            match binding {
                TopBinding::Type(name, abbr) => ctx.insert_ty_abbr_mut(name.as_str(), abbr.clone()),
                TopBinding::Expr(name, ty, _) => {
                    ctx.insert_var_ty_mut(name.as_str(), Some(ty.clone()))
                }
            }
        }
        for binding in self
            .0
            .into_iter()
            .filter(|f| matches!(f, TopBinding::Expr(_, _, _)))
        {
            let TopBinding::Expr(name, ty, expr) = binding else { unreachable!()};
            let expr = expr.map(|x| {
                x.closure_conversion_aux(
                    ctx.clone(),
                    ctx.clone(),
                    &fresh1,
                    &fresh2,
                    &fresh3,
                    &mut work_list,
                )
            });
            work_list.push(TopBinding::Expr(name, ty, expr))
        }
        Module(work_list.into())
    }
}

fn fresh_name_for_lambda(n: &RefCell<usize>) -> String {
    let s = format!("__lambda__{}", *n.borrow()).into();
    *n.borrow_mut() += 1;
    s
}

fn fresh_name_for_env(n: &RefCell<usize>) -> String {
    let s = format!("__env__{}", *n.borrow()).into();
    *n.borrow_mut() += 1;
    s
}

fn fresh_name_for_closure(n: &RefCell<usize>) -> String {
    let s = format!("__closure__{}", *n.borrow()).into();
    *n.borrow_mut() += 1;
    s
}

struct ClosureInfo {
    pub lambda_name: Option<String>,
    pub env_name: Option<String>,
    pub env: Option<Expr>,
    pub expr: Option<Expr>,
}

impl ClosureInfo {
    fn requires_env_capture(&self) -> bool {
        self.env_name.is_some() && self.env.is_some()
    }

    fn requires_lifting(&self) -> bool {
        self.lambda_name.is_some()
    }
}

impl Expr {
    fn rename_and_capture(
        self,
        top_ctx: ContextGroup,
        fresh1: &RefCell<usize>,
        fresh2: &RefCell<usize>,
        _fresh3: &RefCell<usize>,
    ) -> ClosureInfo {
        match self {
            Expr::Abs(_, _) => {
                let fv = top_ctx.free_variables(&self);
                if fv.is_empty() {
                    // do not create closure when there's no free variable
                    ClosureInfo {
                        lambda_name: Some(fresh_name_for_lambda(fresh1)),
                        env_name: None,
                        env: None,
                        expr: Some(self.transform_lambda(&fv)),
                    }
                } else {
                    ClosureInfo {
                        lambda_name: Some(fresh_name_for_lambda(fresh1)),
                        env_name: Some(fresh_name_for_env(fresh2)),
                        env: Some(self.make_env(&fv)),
                        expr: Some(self.transform_lambda(&fv)),
                    }
                }
            }
            _ => ClosureInfo {
                lambda_name: None,
                env_name: None,
                env: None,
                expr: Some(self),
            },
        }
    }

    fn lift_and_update_binding(
        name: String,
        ty: Option<TyRef>,
        expr: P<Expr>,
        mut ctx: ContextGroup,
        top_ctx: ContextGroup,
        fresh1: &RefCell<usize>,
        fresh2: &RefCell<usize>,
        fresh3: &RefCell<usize>,
        top_bindings: &mut Vec<TopBinding>,
        local_bindings: &mut Vec<(String, Option<TyRef>, ExRef)>,
    ) -> ContextGroup {
        let work_list = local_bindings;
        let info = expr.and_then(|x| x.rename_and_capture(top_ctx.clone(), fresh1, fresh2, fresh3));
        if info.requires_env_capture() {
            let ClosureInfo {
                lambda_name: Some(lam_name),
                env_name: Some(env_name),
                env: Some(env),
                expr: Some(f),
            } = info else { unreachable!() };

            // make a closure name
            let closure_name = fresh_name_for_closure(fresh3);

            // lift the lambda body to the top scope
            top_bindings.push(TopBinding::Expr(
                lam_name.clone(),
                ty.unwrap_or_else(|| panic!("lambda expr {name} should be annotated")),
                P(f.clone()),
            ));

            // insert binding for env capture and closure creatation
            // this will keep the properties of ANF
            let closure = P(Expr::Var(closure_name.clone()));
            work_list.extend_from_slice(&[
                (env_name.clone(), None, P(env.clone())),
                (
                    closure_name.clone(),
                    None,
                    P(Self::make_closure(lam_name.as_str(), env_name.as_str())),
                ),
                (name.clone(), None, closure.clone()),
            ]);

            // map the binding name into the closure
            // do not bind the env name because it's useless and may cause variable shadowing
            ctx = ctx.insert_closure_subst(name.as_str(), ());
        } else if info.requires_lifting() {
            let ClosureInfo {
                lambda_name: Some(lam_name),
                env_name: None,
                env: None,
                expr: Some(f),
            } = info else { unreachable!() };

            let lam = P(Expr::Var(lam_name.clone()));

            // lift the lambda body to the top scope
            top_bindings.push(TopBinding::Expr(
                lam_name,
                ty.clone().expect("lambda expr should be annotated"),
                P(f),
            ));

            work_list.push((name.clone(), ty.clone(), lam.clone()));

            ctx = ctx.insert_lambda_subst(name.as_str(), ());
        } else {
            // keep the original binding and
            // change the call site if necessary
            work_list.push((
                name.clone(),
                ty.clone(),
                P(info.expr.unwrap().transform_call_site(ctx.clone())),
            ));

            // update the context
            ctx = ctx.insert_var_ty(name.as_str(), ty)
        }
        ctx
    }

    fn closure_conversion_aux(
        self,
        mut ctx: ContextGroup,
        top_ctx: ContextGroup,
        fresh1: &RefCell<usize>,
        fresh2: &RefCell<usize>,
        fresh3: &RefCell<usize>,
        top_bindings: &mut Vec<TopBinding>,
    ) -> Self {
        let mut work_list = vec![];
        let mut expr = self;
        while let Expr::Let(x, t, e1, e2) = expr {
            ctx = Self::lift_and_update_binding(
                x,
                t,
                e1,
                ctx.clone(),
                top_ctx.clone(),
                fresh1,
                fresh2,
                fresh3,
                top_bindings,
                &mut work_list,
            );

            expr = e2.into_inner()
        }
        expr.fold_bindings(work_list)
    }

    fn transform_lambda(self, fv: &HashSet<String>) -> Self {
        if fv.is_empty() {
            self
        } else {
            let Expr::Abs(params, body) = self else { unreachable!() };
            let mut params = params.into_vec();
            params.insert(0, ("__closure__".into(), None));
            let closure = P(Expr::Var("__closure__".into()));

            let fv = fv.iter().collect_vec();
            let new_body = fv.into_iter().enumerate().rfold(body, |expr, (idx, fv)| {
                P(Expr::Let(
                    fv.clone(),
                    None,
                    P(Expr::Proj(closure.clone(), (idx + 1) as u64)),
                    expr,
                ))
            });
            Expr::Abs(params.into(), new_body)
        }
    }

    fn transform_call_site(self, ctx: ContextGroup) -> Self {
        if let Expr::App(f, _) = &self {
            let Expr::Var(name) = f.as_ref() else { panic!("wrong application") };
            if ctx.lookup_closure_subst(name.as_str()).is_some() {
                return self.transform_closure_call_site(None);
            }
            if ctx.lookup_lambda_subst(name.as_str()).is_some() {
                return self.transform_direct_call_site(None);
            }
            panic!("unknown function {name}")
        } else {
            self
        }
    }

    fn transform_closure_call_site(self, closure: Option<ExRef>) -> Self {
        let Expr::App(ff, args) = self else { unreachable!() };
        Expr::AppClosure(closure.unwrap_or(ff), args)
    }

    fn transform_direct_call_site(self, f: Option<ExRef>) -> Self {
        let Expr::App(ff, args) = self else { unreachable!() };
        Expr::AppDirectly(f.unwrap_or(ff), args)
    }

    fn make_env(&self, fv: &HashSet<String>) -> Self {
        Expr::Inj(
            Some("env".into()),
            fv.iter()
                .map(|name| Expr::Var(name.clone()))
                .collect_vec()
                .into(),
        )
    }

    fn make_closure(f: &str, env: &str) -> Self {
        Expr::Inj(
            Some("closure".into()),
            vec![Expr::Var(f.into()), Expr::Var(env.into())].into(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::core::parse, utils::sexp::Sexp};

    fn closure_conv(input: &str) {
        let module = parse(input).unwrap();
        let module = module.anf().closure_conversion();
        let sexp: Sexp<String> = module.into();
        println!("{sexp}\n");
    }

    #[test]
    fn test_closure_conversion() {
        closure_conv(
            r"
    (def _1 :: unit
     (let c :: int 32 
      (let k 
       (\ (x) c)
        (@ k c))))
    (def _2 :: unit
     (let c :: int 32 
      (let k 
       (\ (x) x)
       (\ (x) x))))
    (def _3 :: unit
     (let c :: int 32 
      (let k 
       (\ (x) x)
        (@ k c))))
",
        );
    }
}
