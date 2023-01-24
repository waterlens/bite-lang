use super::*;

impl Expr {
    pub fn is_value(&self) -> bool {
        matches!(
            self,
            Expr::Unit | Expr::Literal(_) | Expr::Var(_) | Expr::Operator(_)
        )
    }

    pub fn fold_bindings(self, bindings: Vec<(String, Option<TyRef>, ExRef)>) -> Self {
        bindings
            .into_iter()
            .rfold(P(self), |e2, (x, t, e1)| {
                if matches!(e2.as_ref(), Expr::Var(e) if e.as_str() == x) && e1.is_value() {
                    e1
                } else {
                    P(Expr::Let(x, t, e1, e2))
                }
            })
            .into_inner()
    }
}
