use super::*;

impl Module {
    pub fn closure_conversion(self) -> Self {
        let mut work_list = vec![];
        for binding in self
            .0
            .into_iter()
            .filter(|f| matches!(f, TopBinding::Expr(_, _)))
        {
            let TopBinding::Expr(name, expr) = binding else { unreachable!()};
            let expr = expr.map(|x| Self::closure_conversion_aux(x, &mut work_list));
            work_list.push(TopBinding::Expr(name, expr))
        }
        Module(work_list.into())
    }

    fn closure_conversion_aux(mut expr: Expr, bindings: &mut Vec<TopBinding>) -> Expr {
        // let mut work_list = vec![];
        // let mut ctx = Context::new();
        while let Expr::Let(x, t, e1, e2) = &expr {
        }
        todo!()
    }

    fn transform_lambda(expr: &Expr) -> Expr {
        todo!()
    }

    fn transform_call_site(expr: &Expr) -> Expr {
        todo!()
    }

    fn make_closure(ctx: &Expr, expr: &Expr) -> Expr {
        todo!()
    }
}
