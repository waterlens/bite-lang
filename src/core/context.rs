use std::collections::HashSet;

use super::*;

impl<K, V> Clone for Context<K, V>
where
    K: Hash + Eq,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<K, V> Context<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        Context(HashTrieMap::new())
    }

    #[must_use]
    pub fn insert(&self, key: K, value: V) -> Self {
        Context(self.0.insert(key, value))
    }

    pub fn insert_mut(&mut self, key: K, value: V) {
        self.0.insert_mut(key, value)
    }

    pub fn lookup<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(k)
    }
}

impl Context<String, Binding> {
    fn find_type(&self, name: &str) -> Result<&Type, Error> {
        if let Some(Binding::TyAbbr(x)) = self.lookup(name) {
            Ok(x)
        } else {
            Err(anyhow!("{} can't be found in current context", name))
        }
    }

    pub fn compute_type(&self, ty: &Type) -> Result<&Type, Error> {
        match ty {
            Type::Named(s) => self.find_type(s),
            _ => Err(anyhow!("not an named type")),
        }
    }

    pub fn simplify_type<'a>(&'a self, ty: &'a Type) -> &'a Type {
        let mut res = ty;
        while let Ok(ty) = self.compute_type(ty) {
            res = ty;
        }
        res
    }

    pub fn types_eq(&self, t1: &[Type], t2: &[Type]) -> bool {
        t1.len() == t2.len()
            && t1
                .iter()
                .zip(t2.iter())
                .try_fold((), |_, (t1, t2)| {
                    if self.type_eq(t1, t2) {
                        Ok(())
                    } else {
                        Err(())
                    }
                })
                .is_ok()
    }

    pub fn unify<'a>(&self, _t1: &'a Type, _t2: &'a Type) -> Vec<(isize, Type)> {
        todo!()
    }

    pub fn type_eq(&self, t1: &Type, t2: &Type) -> bool {
        let t1 = self.simplify_type(t1);
        let t2 = self.simplify_type(t2);
        use Type::*;
        match (t1, t2) {
            (Unit, Unit) | (Bool, Bool) | (Integer, Integer) | (Str, Str) => true,
            (Arrow(t1, t2, t3), Arrow(t4, t5, t6)) => {
                self.types_eq(t1, t4)
                    && self.type_eq(t2, t5)
                    && t3
                        .as_ref()
                        .zip(t6.as_ref())
                        .map_or(false, |(t3, t6)| self.type_eq(t3, t6))
            }
            (Var(i1), Var(i2)) => i1 == i2,
            (Named(s), t2) if matches!(self.lookup(s), Some(Binding::TyAbbr(_))) => {
                let Some(Binding::TyAbbr(t1)) = self.lookup(s) else { unreachable!( )};
                self.type_eq(t1, t2)
            }
            (t1, Named(s)) if matches!(self.lookup(s), Some(Binding::TyAbbr(_))) => {
                let Some(Binding::TyAbbr(t2)) = self.lookup(s) else { unreachable!( )};
                self.type_eq(t1, t2)
            }
            (Named(s1), Named(s2)) => s1 == s2,
            (Tuple(t1), Tuple(t2)) => self.types_eq(t1, t2),
            (All(x, t1), All(_, t2)) => {
                let ctx = self.insert(x.clone(), Binding::Name);
                ctx.type_eq(t1, t2)
            }
            (Ctor(x, t1), Ctor(y, t2)) if x == y => self.types_eq(t1, t2),
            (Variant(f1), Variant(f2)) if f1.len() == f2.len() => f1
                .iter()
                .zip(f2.iter())
                .try_fold((), |(), ((x, t1), (y, t2))| {
                    if x == y && self.types_eq(t1, t2) {
                        Ok(())
                    } else {
                        Err(())
                    }
                })
                .is_ok(),
            _ => false,
        }
    }

    pub fn free_variables(&self, expr: &Expr) -> HashSet<String> {
        match expr {
            Expr::Unit | Expr::Literal(_) | Expr::Operator(_) => HashSet::new(),
            Expr::Anno(e, _) => self.free_variables(e.as_ref()),
            Expr::Var(x) => {
                let mut fv = HashSet::new();
                if !matches!(self.lookup(x.as_str()), Some(Binding::VarTy(_))) {
                    fv.insert(x.clone());
                }
                fv
            }
            Expr::If(e1, e2, e3) => {
                let fv1 = self.free_variables(e1.as_ref());
                let fv2 = self.free_variables(e2.as_ref());
                let fv3 = self.free_variables(e3.as_ref());
                let fv = fv1
                    .union(&fv2)
                    .cloned()
                    .collect::<HashSet<_>>()
                    .union(&fv3)
                    .cloned()
                    .collect();
                fv
            }
            Expr::Abs(xs, e) => {
                let mut new_ctx = self.clone();
                for (x, _) in xs {
                    new_ctx.insert_mut(x.clone(), Binding::VarTy(None))
                }
                new_ctx.free_variables(e.as_ref())
            }
            Expr::App(e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let fv = e2.iter().fold(fv1, |fv, e| {
                    fv.union(&self.free_variables(e)).cloned().collect()
                });
                fv
            }
            Expr::Inj(_, xs) => xs.iter().fold(HashSet::new(), |fv, e| {
                fv.union(&self.free_variables(e)).cloned().collect()
            }),
            Expr::Proj(e, _) => self.free_variables(&e),
            Expr::Case(e, xs) => {
                let fv1 = self.free_variables(e.as_ref());
                let fv = xs.iter().fold(fv1, |fv, (_, _, e)| {
                    fv.union(&self.free_variables(e)).cloned().collect()
                });
                fv
            }
            Expr::Let(x, _, e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let new_ctx = self.insert(x.clone(), Binding::VarTy(None));
                let fv = new_ctx.free_variables(e2.as_ref());
                fv1.union(&fv).cloned().collect()
            }
            Expr::Try(x, e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let new_ctx = self.insert(x.clone(), Binding::VarTy(None));
                let fv = new_ctx.free_variables(e2.as_ref());
                fv1.union(&fv).cloned().collect()
            }
            Expr::Resume(e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let fv2 = self.free_variables(e2.as_ref());
                fv1.union(&fv2).cloned().collect()
            }
            Expr::Raise(e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let fv2 = self.free_variables(e2.as_ref());
                fv1.union(&fv2).cloned().collect()
            }
            Expr::TAbs(_, e) => self.free_variables(e.as_ref()),
            Expr::TApp(e, _) => self.free_variables(e.as_ref()),
        }
    }
}
