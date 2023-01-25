use std::vec;

use itertools::Itertools;

use super::*;

impl Module {
    pub fn closure_conversion(self) -> Self {
        ClosureConversion::new(self).closure_conversion()
    }
}

fn fresh_name_for_lambda(n: &RefCell<usize>) -> String {
    let s = format!("%lambda{}", *n.borrow()).into();
    *n.borrow_mut() += 1;
    s
}

pub struct ClosureConversion {
    pub top_ctx: RefCell<ContextGroup>,
    pub ctx: RefCell<ContextGroup>,
    pub fresh: RefCell<usize>,
    pub module: Module,
    pub top_bindings: RefCell<Vec<TopBinding>>,
}

impl ClosureConversion {
    fn new(module: Module) -> Self {
        let mut top_ctx = ContextGroup::new();
        let fresh = RefCell::new(0);
        for binding in module.0.iter() {
            match binding {
                TopBinding::Type(name, abbr) => {
                    top_ctx.insert_ty_abbr_mut(name.as_str(), abbr.clone())
                }
                TopBinding::Expr(name, ty, _) => {
                    top_ctx.insert_var_ty_mut(name.as_str(), Some(ty.clone()))
                }
            }
        }
        let top_bindings = RefCell::new(vec![].to_vec());
        let top_ctx = RefCell::new(top_ctx);
        let ctx = top_ctx.clone();
        Self {
            top_ctx,
            ctx,
            fresh,
            module,
            top_bindings,
        }
    }

    fn close_lambda(&self, expr: Expr) -> Expr {
        use Expr::*;
        let f = |x| self.close_lambda(x);
        match expr {
            Unit | Literal(_) | Var(_) | Operator(_) => expr,
            Anno(e, t) => Anno(e.map(f), t),
            If(e1, e2, e3) => If(e1.map(f), e2.map(f), e3.map(f)),
            Abs(params, e) => {
                let mut params = params.into_vec();
                let mut new_ctx = self.top_ctx.borrow().clone();
                for (name, ty) in &params {
                    new_ctx.insert_var_ty_mut(name.as_str(), ty.clone())
                }
                let fv = new_ctx.free_variables(&e);
                let fv = fv.iter().collect_vec();
                let e = e.map(f);
                if fv.is_empty() {
                    return Expr::Abs(params.into(), e);
                }
                params.insert(
                    0,
                    (
                        "%closure".into(),
                        Some(Type::Ctor("closure".into(), [].to_vec().into()).into()),
                    ),
                );
                let closure = P(Expr::Var("%closure".into()));
                let next_e = fv
                    .iter()
                    .copied()
                    .enumerate()
                    .rfold(e, |expr, (idx, fv)| {
                        P(Expr::Let(
                            fv.clone(),
                            None,
                            P(Expr::Proj(closure.clone(), (idx + 1) as u64)),
                            expr,
                        ))
                    });

                let mut closure_fields = vec![Expr::Abs(params.into(), next_e)];
                closure_fields.extend(fv.iter().copied().map(|name| Expr::Var(name.clone())));
                Expr::Inj(Some("closure".into()), closure_fields.into())
            }
            App(e1, e2) => App(e1.map(f), e2.into_iter().map(f).collect_vec().into()),
            AppClosure(_, _) => unimplemented!("only used after closure conversion"),
            AppDirectly(_, _) => unimplemented!("only used after closure conversion"),
            Inj(x, e) => Inj(x, e.into_iter().map(f).collect_vec().into()),
            Proj(e, x) => Proj(e.map(f), x),
            Case(e, cases) => Case(
                e.map(f),
                cases
                    .into_iter()
                    .map(|(x, pat, e)| (x, pat, e.map(f)))
                    .collect_vec()
                    .into(),
            ),
            Let(x, t, e1, e2) => Let(x, t, e1.map(f), e2.map(f)),
            Try(x, e1, e2) => Try(x, e1.map(f), e2.map(f)),
            Resume(e1, e2) => Resume(e1.map(f), e2.map(f)),
            Raise(e1, e2) => Raise(e1.map(f), e2.map(f)),
            TAbs(t, e) => TAbs(t, e.map(f)),
            TApp(e, t) => TApp(e.map(f), t),
        }
    }

    fn hoist_lambda(&self, expr: Expr) -> Expr {
        use Expr::*;
        let f = |x| self.hoist_lambda(x);
        match expr {
            Unit | Literal(_) | Var(_) | Operator(_) => expr,
            Anno(e, t) => Anno(e.map(f), t),
            If(e1, e2, e3) => If(e1.map(f), e2.map(f), e3.map(f)),
            Abs(xs, e) => {
                let expr = Abs(xs, e.map(f));
                let name = fresh_name_for_lambda(&self.fresh);
                self.top_bindings.borrow_mut().push(TopBinding::Expr(
                    name.clone(),
                    P(Type::Hole),
                    P(expr),
                ));
                Expr::Var(name)
            }
            App(e1, e2) => App(e1.map(f), e2.into_iter().map(f).collect_vec().into()),
            AppClosure(_, _) => unimplemented!("only used after closure conversion"),
            AppDirectly(_, _) => unimplemented!("only used after closure conversion"),
            Inj(x, e) => Inj(x, e.into_iter().map(f).collect_vec().into()),
            Proj(e, x) => Proj(e.map(f), x),
            Case(e, cases) => Case(
                e.map(f),
                cases
                    .into_iter()
                    .map(|(x, pat, e)| (x, pat, e.map(f)))
                    .collect_vec()
                    .into(),
            ),
            Let(x, t, e1, e2) => Let(x, t, e1.map(f), e2.map(f)),
            Try(x, e1, e2) => Try(x, e1.map(f), e2.map(f)),
            Resume(e1, e2) => Resume(e1.map(f), e2.map(f)),
            Raise(e1, e2) => Raise(e1.map(f), e2.map(f)),
            TAbs(t, e) => TAbs(t, e.map(f)),
            TApp(e, t) => TApp(e.map(f), t),
        }
    }

    fn closure_conversion(self) -> Module {
        for binding in self.module.0.iter() {
            match binding {
                TopBinding::Type(_, _) => self.top_bindings.borrow_mut().push(binding.clone()),
                TopBinding::Expr(x, t, e) => {
                    let e = e
                        .clone()
                        .map(|x| self.close_lambda(x))
                        .map(|x| self.hoist_lambda(x));
                    let new = TopBinding::Expr(x.clone(), t.clone(), e);
                    self.top_bindings.borrow_mut().push(new);
                }
            }
        }
        Module(self.top_bindings.take().into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::core::parse, utils::sexp::Sexp};

    fn closure_conv(input: &str) {
        let module = parse(input).unwrap();
        let module = ClosureConversion::new(module).closure_conversion();
        let sexp: Sexp<String> = module.into();
        println!("{sexp}\n");
    }

    #[test]
    fn test_closure_conversion() {
        closure_conv(
            r"
    (def adder :: ([int] -> ([int] -> int))
     (\ [x] (\ [y] (@ + x y))))
",
        );
    }
}
