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
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
        ty: Option<TyRef>,
        expr: Expr,
    ) -> Expr {
        binding.borrow_mut().push((name.into(), ty, expr));
        Expr::Var(name.into())
    }

    fn add_fresh_binding(
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
        ty: Option<TyRef>,
        expr: Expr,
    ) -> Expr {
        let name = gen_sym(fresh);
        Self::add_binding(name.as_str(), binding, ty, expr)
    }

    fn normalize_expr_without_fresh(
        self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
    ) -> Self {
        match self {
            Expr::Unit | Expr::Literal(_) | Expr::Var(_) | Expr::Operator(_) => {
                unreachable!()
            }
            Expr::Anno(e, ty) => {
                // regard it as `let x: ty = e in x`
                let e = e.map(|x| x.normalize_aux_without_fresh(fresh, binding));
                Self::add_fresh_binding(fresh, binding, Some(ty), e.into_inner())
            }
            Expr::If(e1, e2, e3) => {
                let e1 = e1.map(|x| x.normalize_aux(fresh, binding));

                let e2 = e2.map(|x| x.normalize(fresh));
                let e3 = e3.map(|x| x.normalize(fresh));

                let expr = Expr::If(e1, e2, e3);
                expr
            }
            Expr::Abs(x, e) => Expr::Abs(x.clone(), e.map(|x| x.normalize(fresh))),
            Expr::App(f, xs) => {
                let f = f.map(|x| x.normalize_aux(fresh, binding));
                let xs = xs
                    .into_iter()
                    .map(|x| x.normalize_aux(fresh, binding))
                    .collect();
                let expr = Expr::App(f, xs);
                expr
            }
            Expr::Inj(x, xs) => {
                let xs = xs
                    .into_iter()
                    .map(|x| x.normalize_aux(fresh, binding))
                    .collect();
                let expr = Expr::Inj(x.clone(), xs);
                expr
            }
            Expr::Proj(e, idx) => {
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let expr = Expr::Proj(e, idx);
                expr
            }
            Expr::Case(e, xs) => {
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let xs = xs
                    .into_iter()
                    .map(|(x, ps, e)| (x, ps, e.map(|x| x.normalize(fresh))))
                    .collect();
                let expr = Expr::Case(e, xs);
                expr
            }
            Expr::Let(x, ty, e1, e2) if matches!(e2.as_ref(), Expr::Var(y) if x.as_str() == y.as_str()) =>
            {
                // let x: ty = e1 in x
                // if possible, don't introduce a fresh variable in e1
                let e1 = e1.map(|x| x.normalize_aux_without_fresh(fresh, binding));
                Self::add_binding(x.as_str(), binding, ty.clone(), e1.into_inner())
            }
            Expr::Let(x, ty, e1, e2) => {
                // if possible, don't introduce a fresh variable in e1
                let e1 = e1.map(|x| x.normalize_aux_without_fresh(fresh, binding));
                Self::add_binding(x.as_str(), binding, ty.clone(), e1.into_inner());
                let e2 = e2.map(|x| x.normalize_aux(fresh, binding));
                e2.into_inner()
            }
            Expr::Try(x, e1, e2) => {
                let e1 = e1.map(|x| x.normalize_aux(fresh, binding));
                Self::add_binding(x.as_str(), binding, None, e1.into_inner());
                let e2 = e2.map(|x| x.normalize_aux(fresh, binding));
                e2.into_inner()
            }
            Expr::Resume(k, e) => {
                let k = k.map(|x| x.normalize_aux(fresh, binding));
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let expr = Expr::Resume(k, e);
                expr
            }
            Expr::Raise(k, e) => {
                let k = k.map(|x| x.normalize_aux(fresh, binding));
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let expr = Expr::Raise(k, e);
                expr
            }
            Expr::TAbs(xs, e) => {
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let expr = Expr::TAbs(xs.clone(), e);
                expr
            }
            Expr::TApp(e, xs) => {
                let e = e.map(|x| x.normalize_aux(fresh, binding));
                let expr = Expr::TApp(e, xs.clone());
                expr
            }
        }
    }

    fn normalize_expr(
        self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
    ) -> Self {
        let expr = self.normalize_expr_without_fresh(fresh, binding);
        Self::add_fresh_binding(fresh, binding, None, expr)
    }

    fn is_value(&self) -> bool {
        matches!(
            self,
            Expr::Unit | Expr::Literal(_) | Expr::Var(_) | Expr::Operator(_)
        )
    }

    fn normalize_value(&self) -> Option<Self> {
        match self {
            x if x.is_value() => Some(self.clone()),
            _ => None,
        }
    }

    fn normalize_aux(
        self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
    ) -> Self {
        self.normalize_value()
            .unwrap_or_else(|| self.normalize_expr(fresh, binding))
    }

    fn normalize_aux_without_fresh(
        self,
        fresh: &RefCell<usize>,
        binding: &RefCell<Vec<(String, Option<TyRef>, Expr)>>,
    ) -> Self {
        self.normalize_value()
            .unwrap_or_else(|| self.normalize_expr_without_fresh(fresh, binding))
    }

    fn normalize(self, fresh: &RefCell<usize>) -> Self {
        let binding = RefCell::new(vec![]);
        let expr = self.normalize_aux(fresh, &binding);
        let result = binding
            .into_inner()
            .into_iter()
            .rfold(expr, |e2, (x, t, e1)| {
                if matches!(&e2, Expr::Var(e) if e.as_str() == x) && e1.is_value() {
                    e1
                } else {
                    Expr::Let(x, t, P(e1), P(e2))
                }
            });
        result
    }

    pub fn anf(self) -> Self {
        self.normalize(&RefCell::new(0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::core::*;
    use crate::utils::sexp::Sexp;

    fn normalize(input: &str) {
        let expr = parse_expr(input).unwrap();
        let expr = expr.anf();
        let sexp: Sexp<String> = expr.into();
        println!("{sexp}\n");
    }

    #[test]
    fn test_normalizer() {
        normalize(r"(@ + (let x :: int 1 x) (let x :: int 1 x))");

        normalize(r"(@ (@ f g) (@ h x) 3)");

        normalize(
            r"
        (let f (\ (x) x)
            (let _ (@ f 10)
                (let _ (@ f true)
                    42)))
        ",
        );

        normalize(
            r"
        (let x (if c1 (@ + 5 5) (@ * 6 2))
            (let y (if c2 (@ * x 3) (@ + x 5))
                (@ + x y)))",
        );
    }
}
