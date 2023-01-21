use anyhow::{anyhow, Error};
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use rpds::HashTrieMap;
use smartstring::alias::String;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

pub(crate) mod normalizer;
pub(crate) mod conversion;

pub static OP_NAME: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "-", "!", "*", "/", "%", "+", "==", "!=", "<=", ">=", "<", ">",
    ])
});

pub type TyRef = Rc<Type>;
#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Str,
    Integer,
    Bool,
    Var(isize),
    Named(String),
    All(String, TyRef),
    Arrow(Vec<Type>, TyRef, Option<TyRef>),
    Variant(Vec<(String, Vec<Type>)>),
    Tuple(Vec<Type>),
    Ctor(String, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Operator(pub &'static str);

pub type ExRef = Rc<Expr>;
#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Anno(ExRef, TyRef),
    Literal(Box<Literal>),
    Var(String),
    Operator(Operator),
    If(ExRef, ExRef, ExRef),
    Abs(Vec<(String, Option<TyRef>)>, ExRef),
    App(ExRef, Vec<Expr>),
    Inj(Option<String>, Vec<Expr>),
    Proj(ExRef, i64),
    Case(ExRef, Vec<(String, Vec<Option<String>>, Expr)>),
    Let(String, Option<TyRef>, ExRef, ExRef),
    Try(String, ExRef, ExRef),
    Resume(ExRef, ExRef),
    Raise(ExRef, ExRef),
    TAbs(Vec<String>, ExRef),
    TApp(ExRef, Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum TopBinding {
    Type(String, Type),
    Expr(String, Expr),
}

#[derive(Debug, Clone)]
pub struct Module(pub Vec<TopBinding>);

#[derive(Debug)]
pub struct Context<K, V>(HashTrieMap<K, V>)
where
    K: Hash + Eq;

pub enum Binding {
    Name,
    VarTy(Option<Type>),
    TyAlias(Type),
}

impl Type {
    pub fn pack(self) -> TyRef {
        Rc::new(self)
    }

    fn map_aux<F1, F2>(&self, c: isize, f1: F1, f2: F2) -> Self
    where
        F1: Fn(isize, isize) -> Self + Clone,
        F2: Fn(isize, &str) -> Self + Clone,
    {
        use Type::*;
        match self {
            Unit | Str | Integer | Bool => self.clone(),
            Var(x) => f1(c, *x),
            Named(x) => f2(c, x.as_str()),
            All(x, y) => All(x.clone(), y.map_aux(c + 1, f1, f2).pack()),
            Arrow(x, y, z) => Arrow(
                x.iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
                y.map_aux(c, f1.clone(), f2.clone()).pack(),
                z.as_ref()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()).pack()),
            ),
            Variant(x) => Variant(
                x.iter()
                    .map(|(s, t)| {
                        (
                            s.clone(),
                            t.iter()
                                .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                                .collect(),
                        )
                    })
                    .collect(),
            ),
            Tuple(x) => Tuple(
                x.iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
            ),
            Ctor(x, y) => Ctor(
                x.clone(),
                y.iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
            ),
        }
    }

    fn map_index<F>(&self, c: isize, f: F) -> Self
    where
        F: Fn(isize, isize) -> Self + Clone,
    {
        self.map_aux(c, f, |_, x| Type::Named(x.into()))
    }

    fn map_named<F>(&self, c: isize, f: F) -> Self
    where
        F: Fn(isize, &str) -> Self + Clone,
    {
        self.map_aux(c, |_, x| Type::Var(x), f)
    }

    pub fn to_locally_nameless(self) -> Type {
        match self {
            Type::Unit | Type::Str | Type::Integer | Type::Bool | Type::Var(_) | Type::Named(_) => {
                self
            }
            Type::All(x, y) => Type::All(
                x.clone(),
                y.var_close(x.as_str()).to_locally_nameless().pack(),
            ),
            Type::Arrow(x, y, z) => Type::Arrow(
                x.into_iter().map(|x| x.to_locally_nameless()).collect(),
                y.as_ref().clone().to_locally_nameless().pack(),
                z.map(|z| z.as_ref().clone().to_locally_nameless().pack()),
            ),
            Type::Variant(xs) => Type::Variant(
                xs.iter()
                    .map(|(x, y)| {
                        (
                            x.clone(),
                            y.iter().map(|y| y.clone().to_locally_nameless()).collect(),
                        )
                    })
                    .collect(),
            ),
            Type::Tuple(xs) => {
                Type::Tuple(xs.iter().map(|x| x.clone().to_locally_nameless()).collect())
            }
            Type::Ctor(x, ys) => Type::Ctor(
                x.clone(),
                ys.iter().map(|y| y.clone().to_locally_nameless()).collect(),
            ),
        }
    }

    fn shift_above(&self, d: isize, c: isize) -> Self {
        self.map_index(c, |c, x| {
            if x >= c {
                Type::Var(x + d)
            } else {
                Type::Var(x)
            }
        })
    }

    fn shift(&self, d: isize) -> Self {
        self.shift_above(d, 0)
    }

    fn subst_n(&self, n: isize, ty: &Type) -> Self {
        self.map_index(n, |c, x| if c == x { ty.shift(c) } else { Type::Var(x) })
    }

    pub fn subst(&self, ty: &Type) -> Self {
        self.subst_n(0, &ty.shift(1)).shift(-1)
    }

    fn open_n(&self, n: isize, ty: &Type) -> Self {
        self.map_index(n, |c, x| if c == x { ty.clone() } else { Type::Var(x) })
    }

    pub fn open(&self, ty: &Type) -> Self {
        self.open_n(0, ty)
    }

    fn var_close_n(&self, n: isize, name: &str, x: isize) -> Self {
        self.map_named(n, |c, s| {
            if s == name {
                Type::Var(x + c - n)
            } else {
                Type::Named(s.into())
            }
        })
    }

    pub fn var_close(&self, name: &str) -> Self {
        self.var_close_n(0, name, 0)
    }

    pub fn free_names(&self) -> IndexSet<String> {
        let ls = RefCell::new(IndexSet::new());
        self.map_named(0, |_, s| {
            ls.borrow_mut().insert(s.into());
            Type::Named(s.into())
        });
        ls.take()
    }

    fn remove_quantifiers(&self) -> (Vec<String>, &Type) {
        let mut v = vec![];
        let mut ty = self;
        while let Type::All(x, inner) = ty {
            v.push(x.clone());
            ty = inner;
        }
        (v, ty)
    }

    pub fn normalize(&self) -> Type {
        let (names, inner) = self.remove_quantifiers();
        let free = RefCell::new(IndexSet::new());
        inner.map_index(0, |c, x| {
            if x >= c {
                free.borrow_mut().insert(x - c);
            }
            Type::Var(x)
        });
        let free: Vec<_> = free.take().into_iter().collect();
        let len = free.len();
        let map: HashMap<isize, isize> = free
            .into_iter()
            .rev()
            .enumerate()
            .map(|(idx, x)| (x, idx as isize))
            .collect();
        let new_inner = inner.map_index(0, |c, x| {
            if let Some(to) = map.get(&(x - c)) {
                Type::Var(*to)
            } else {
                Type::Var(x)
            }
        });
        names[..len].iter().rfold(new_inner, |inner, name| {
            Type::All(name.clone(), inner.pack())
        })
    }
}

impl Expr {
    pub fn pack(self) -> ExRef {
        Rc::new(self)
    }
}

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
        if let Some(Binding::TyAlias(x)) = self.lookup(name) {
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
            (Named(s), t2) if matches!(self.lookup(s), Some(Binding::TyAlias(_))) => {
                let Some(Binding::TyAlias(t1)) = self.lookup(s) else { unreachable!( )};
                self.type_eq(t1, t2)
            }
            (t1, Named(s)) if matches!(self.lookup(s), Some(Binding::TyAlias(_))) => {
                let Some(Binding::TyAlias(t2)) = self.lookup(s) else { unreachable!( )};
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hamt() {
        let mut h0 = HashTrieMap::new();
        h0.insert_mut(1, "1");
        h0.insert_mut(2, "2");
        let h1 = h0.insert(2, "3");
        let h2 = h1.insert(2, "4");
        println!("{h0} {h1} {h2}")
    }

    #[test]
    fn test_to_nameless() {
        use crate::parser::core::*;

        let ty = parse_type(r"(forall (a b) ([a] -> ([b] -> a / c)))").unwrap();
        println!("{ty:#?}");
        let ty = ty.to_locally_nameless();
        println!("{ty:#?}");
    }

    #[test]
    fn test_type_normal_form() {
        use crate::parser::core::*;

        let ty = parse_type(r"(forall (a) (forall (b c) ([b] -> c)))").unwrap();
        let ty = ty.to_locally_nameless();
        println!("{ty:#?}");
        let ty = ty.normalize();
        println!("{ty:#?}");
    }
}
