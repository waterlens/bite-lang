use std::{usize, vec};

use super::*;

fn gen_sym(fresh: &RefCell<usize>) -> String {
    let n = *fresh.borrow();
    (*fresh.borrow_mut()) += 1;
    format!("%{n}").into()
}

impl Expr {
    fn add_binding(
        name: &str,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
        ty: Option<TyRef>,
        expr: ExRef,
    ) -> Self {
        binding.borrow_mut().push((name.into(), ty, expr));
        Expr::Var(name.into())
    }

    fn add_fresh_binding(
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
        ty: Option<TyRef>,
        expr: ExRef,
    ) -> Self {
        let name = gen_sym(fresh);
        Self::add_binding(name.as_str(), binding, ty, expr)
    }

    fn normalize_expr_without_fresh(
        &self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
    ) -> Self {
        match self {
            Expr::Unit | Expr::Literal(_) | Expr::Var(_) | Expr::Operator(_) => {
                unreachable!()
            }
            Expr::Anno(e, ty) => {
                // regard it as `let x: ty = e in x`
                let e = e.normalize_aux_without_fresh(fresh, binding).pack();
                Self::add_fresh_binding(fresh, binding, Some(ty.clone()), e)
            }
            Expr::If(e1, e2, e3) => {
                let e1 = e1.normalize_aux(fresh, binding).pack();

                let e2 = e2.normalize(fresh);
                let e3 = e3.normalize(fresh);

                let expr = Expr::If(e1, e2.pack(), e3.pack());
                expr
            }
            Expr::Abs(x, e) => Expr::Abs(x.clone(), e.normalize(fresh).pack()),
            Expr::App(f, xs) => {
                let f = f.normalize_aux(fresh, binding).pack();
                let xs = xs.iter().map(|x| x.normalize_aux(fresh, binding)).collect();
                let expr = Expr::App(f, xs);
                expr
            }
            Expr::Inj(x, xs) => {
                let xs = xs.iter().map(|x| x.normalize_aux(fresh, binding)).collect();
                let expr = Expr::Inj(x.clone(), xs);
                expr
            }
            Expr::Proj(e, idx) => {
                let e = e.normalize_aux(fresh, binding).pack();
                let expr = Expr::Proj(e, *idx);
                expr
            }
            Expr::Case(e, xs) => {
                let e = e.normalize_aux(fresh, binding).pack();
                let xs = xs
                    .iter()
                    .map(|(x, ps, e)| (x.clone(), ps.clone(), e.normalize(fresh)))
                    .collect();
                let expr = Expr::Case(e, xs);
                expr
            }
            Expr::Let(x, ty, e1, e2) if matches!(e2.as_ref(), Expr::Var(y) if x == y) => {
                // let x: ty = e1 in x
                // if possible, don't introduce a fresh variable in e1
                let e1 = e1.normalize_aux_without_fresh(fresh, binding).pack();
                Self::add_binding(x.as_str(), binding, ty.clone(), e1)
            }
            Expr::Let(x, ty, e1, e2) => {
                // if possible, don't introduce a fresh variable in e1
                let e1 = e1.normalize_aux_without_fresh(fresh, binding).pack();
                Self::add_binding(x.as_str(), binding, ty.clone(), e1);
                let e2 = e2.normalize_aux(fresh, binding);
                e2
            }
            Expr::Try(x, e1, e2) => {
                let e1 = e1.normalize_aux(fresh, binding).pack();
                Self::add_binding(x.as_str(), binding, None, e1);
                let e2 = e2.normalize_aux(fresh, binding);
                e2
            }
            Expr::Resume(k, e) => {
                let k = k.normalize_aux(fresh, binding).pack();
                let e = e.normalize_aux(fresh, binding).pack();
                let expr = Expr::Resume(k, e);
                expr
            }
            Expr::Raise(k, e) => {
                let k = k.normalize_aux(fresh, binding).pack();
                let e = e.normalize_aux(fresh, binding).pack();
                let expr = Expr::Raise(k, e);
                expr
            }
            Expr::TAbs(xs, e) => {
                let e = e.normalize_aux(fresh, binding).pack();
                let expr = Expr::TAbs(xs.clone(), e);
                expr
            }
            Expr::TApp(e, xs) => {
                let e = e.normalize_aux(fresh, binding).pack();
                let expr = Expr::TApp(e, xs.clone());
                expr
            }
        }
    }

    fn normalize_expr(
        &self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
    ) -> Self {
        let expr = self.normalize_expr_without_fresh(fresh, binding).pack();
        Self::add_fresh_binding(fresh, binding, None, expr)
    }

    fn normalize_value(&self) -> Option<Self> {
        match self {
            Expr::Unit | Expr::Literal(_) | Expr::Var(_) | Expr::Operator(_) => Some(self.clone()),
            _ => None,
        }
    }

    fn normalize_aux(
        &self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
    ) -> Self {
        self.normalize_value()
            .unwrap_or_else(|| self.normalize_expr(fresh, binding))
    }

    fn normalize_aux_without_fresh(
        &self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, ExRef)>>,
    ) -> Self {
        self.normalize_value()
            .unwrap_or_else(|| self.normalize_expr_without_fresh(fresh, binding))
    }

    fn normalize(&self, fresh: &RefCell<usize>) -> Self {
        let binding = RefCell::new(vec![]);
        let expr = self.normalize_aux(fresh, &binding);
        binding
            .into_inner()
            .into_iter()
            .rfold(expr, |e2, (x, t, e1)| Expr::Let(x, t, e1, e2.pack()))
    }

    pub fn anf(&self) -> Self {
        self.normalize(&RefCell::new(0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::sexp::Sexp;

    #[test]
    fn test_normalizer() {
        use crate::parser::core::*;

        let expr = parse_expr(r"(@ + (let x :: int 1 x) (let x :: int 1 x))").unwrap();
        let expr = expr.anf();
        let sexp: Sexp<String> = expr.into();
        println!("{sexp}\n");

        let expr = parse_expr(r"(@ (@ f g) (@ h x) 3)").unwrap();
        let expr = expr.anf();
        let sexp: Sexp<String> = expr.into();
        println!("{sexp}\n");

        let expr = parse_expr(
            r"
    (let x (if c1 (@ + 5 5) (@ * 6 2))
        (let y (if c2 (@ * x 3) (@ + x 5))
            (@ + x y)))",
        )
        .unwrap();
        let expr = expr.anf();
        let sexp: Sexp<String> = expr.into();
        println!("{sexp}\n");
    }
}
