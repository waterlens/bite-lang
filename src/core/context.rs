use super::*;

impl<K, V> Clone for Context<K, V>
where
    K: Hash + Eq,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl Clone for ContextGroup {
    fn clone(&self) -> Self {
        Self {
            ctx_group: self.ctx_group.clone(),
        }
    }
}

impl Default for ContextGroup {
    fn default() -> Self {
        Self {
            ctx_group: Context::new(),
        }
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

    #[must_use]
    pub fn insert_or_update<F>(&self, key: K, f: F) -> Self
    where
        F: FnOnce(Option<&V>) -> V,
    {
        let value = f(self.lookup(&key));
        self.insert(key, value)
    }

    #[must_use]
    pub fn insert_or_update_mut<F>(&mut self, key: K, f: F)
    where
        F: FnOnce(Option<&V>) -> V,
    {
        let value = f(self.lookup(&key));
        self.insert_mut(key, value)
    }

    pub fn lookup<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(k)
    }
}

macro_rules! make_lookup {
    ($name: ident, $field: ident, $ty: ty) => {
        pub fn $name(&self, name: &str) -> Option<&$ty> {
            let binding_group = self.ctx_group.lookup(name);
            binding_group.as_ref().and_then(|x| x.$field.as_ref())
        }
    };
}

macro_rules! make_insert {
    ($name: ident, $field: ident, $ty: ty) => {
        pub fn $name(&self, name: &str, value: $ty) -> Self {
            ContextGroup {
                ctx_group: self.ctx_group.insert_or_update(name.into(), |x| {
                    let prev = x.map_or_else(|| Default::default(), |k| k.clone());
                    BindingGroup {
                        $field: Some(value),
                        ..prev
                    }
                }),
            }
        }
    };
}

macro_rules! make_insert_mut {
    ($name: ident, $field: ident, $ty: ty) => {
        pub fn $name(&mut self, name: &str, value: $ty) {
            self.ctx_group.insert_or_update_mut(name.into(), |x| {
                let prev = x.map_or_else(|| Default::default(), |k| k.clone());
                BindingGroup {
                    $field: Some(value),
                    ..prev
                }
            })
        }
    };
}

impl ContextGroup {
    pub fn new() -> Self {
        Default::default()
    }

    make_lookup!(lookup_ty_abbr, ty_abbr, TyRef);
    make_lookup!(lookup_var_ty, var_ty, Option<TyRef>);
    make_lookup!(lookup_closure_subst, closure_subst, ());
    make_lookup!(lookup_lambda_subst, lambda_subst, ());
    make_lookup!(lookup_operator_ty, op_ty, TyRef);
    make_insert!(insert_ty_abbr, ty_abbr, TyRef);
    make_insert!(insert_var_ty, var_ty, Option<TyRef>);
    make_insert!(insert_closure_subst, closure_subst, ());
    make_insert!(insert_lambda_subst, lambda_subst, ());
    make_insert_mut!(insert_ty_abbr_mut, ty_abbr, TyRef);
    make_insert_mut!(insert_var_ty_mut, var_ty, Option<TyRef>);
    make_insert_mut!(insert_closure_subst_mut, closure_subst, ());
    make_insert_mut!(insert_lambda_subst_mut, lambda_subst, ());

    fn find_type(&self, name: &str) -> Result<TyRef, Error> {
        if let Some(x) = self.lookup_ty_abbr(name) {
            Ok(x.clone())
        } else {
            Err(anyhow!("{} can't be found in current context", name))
        }
    }

    pub fn compute_type(&self, ty: &Type) -> Result<TyRef, Error> {
        match ty {
            Type::Named(s) => self.find_type(s),
            _ => Err(anyhow!("not an named type")),
        }
    }

    pub fn simplify_type(&self, ty: &Type) -> TyRef {
        let mut res = P(ty.clone());
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

    pub fn type_eq(&self, t1: &Type, t2: &Type) -> bool {
        let t1 = self.simplify_type(t1);
        let t1 = t1.as_ref();
        let t2 = self.simplify_type(t2);
        let t2 = t2.as_ref();
        use Type::*;
        match (t1, t2) {
            (Unit, Unit) | (Bool, Bool) | (Integer, Integer) | (Str, Str) => true,
            (Arrow(t1, t2, t3), Arrow(t4, t5, t6)) => {
                self.types_eq(t1, t4)
                    && self.type_eq(t2, t5)
                    && t3
                        .as_ref()
                        .zip(t6.as_ref())
                        .map_or(false, |(t3, t6)| self.types_eq(t3, t6))
            }
            (Var(i1), Var(i2)) => i1 == i2,
            (Named(s), t2) if matches!(self.lookup_ty_abbr(s), Some(_)) => {
                let Some(t1) = self.lookup_ty_abbr(s) else { unreachable!( )};
                self.type_eq(t1.as_ref(), t2)
            }
            (t1, Named(s)) if matches!(self.lookup_ty_abbr(s), Some(_)) => {
                let Some(t2) = self.lookup_ty_abbr(s) else { unreachable!( )};
                self.type_eq(t1, t2.as_ref())
            }
            (Named(s1), Named(s2)) => s1 == s2,
            (Tuple(t1), Tuple(t2)) => self.types_eq(t1.as_ref(), t2.as_ref()),
            (All(_, t1), All(_, t2)) => self.type_eq(t1.as_ref(), t2.as_ref()),
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
                if !matches!(self.lookup_var_ty(x.as_str()), Some(_)) {
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
                for (x, ty) in xs {
                    new_ctx.insert_var_ty_mut(x.as_str(), ty.clone())
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
            Expr::AppClosure(e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let fv = e2.iter().fold(fv1, |fv, e| {
                    fv.union(&self.free_variables(e)).cloned().collect()
                });
                fv
            }
            Expr::AppDirectly(e1, e2) => {
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
            Expr::Let(x, t, e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let new_ctx = self.insert_var_ty(x.as_str(), t.clone());
                let fv = new_ctx.free_variables(e2.as_ref());
                fv1.union(&fv).cloned().collect()
            }
            Expr::Try(x, e1, e2) => {
                let fv1 = self.free_variables(e1.as_ref());
                let new_ctx = self.insert_var_ty(x.as_str(), None);
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
