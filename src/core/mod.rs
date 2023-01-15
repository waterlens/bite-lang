use anyhow::{anyhow, Error};
use once_cell::sync::Lazy;
use smartstring::alias::String;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

pub(crate) static OP_NAME: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from([
        "-", "!", "*", "/", "%", "+", "==", "!=", "<=", ">=", "<", ">",
    ])
});

pub(crate) type TyRef = Rc<Type>;
#[derive(Debug, Clone)]
pub(crate) enum Type {
    Unit,
    Str,
    Integer,
    Bool,
    Var(isize),
    Named(String),
    All(Vec<String>, TyRef),
    Arrow(TyRef, TyRef, Option<TyRef>),
    Variant(Vec<(String, Vec<Type>)>),
    Tuple(Vec<Type>),
    Ctor(String, Vec<Type>),
}

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Str(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub(crate) struct Operator {
    pub(crate) name: &'static str,
}

pub(crate) type ExRef = Rc<Expr>;
#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Unit,
    Anno(ExRef, TyRef),
    Literal(Box<Literal>),
    Var(String),
    Operator(Operator),
    If(ExRef, ExRef, ExRef),
    Abs(Vec<(String, Type)>, ExRef),
    App(ExRef, Vec<Expr>),
    Inj(Option<String>, Vec<Expr>),
    Proj(ExRef, isize),
    Case(ExRef, Vec<(String, Vec<Option<String>>, Expr)>),
    Let(Option<String>, ExRef, ExRef),
    Try(ExRef, Option<String>, ExRef),
    Resume(ExRef, ExRef),
    Raise(ExRef, ExRef),
    TAbs(Vec<String>, ExRef),
    TApp(ExRef, Vec<Type>),
}

#[derive(Debug, Clone)]
pub(crate) enum TopBinding {
    Type(String, Type),
    Expr(Option<String>, Expr),
}

#[derive(Debug, Clone)]
pub(crate) struct Module(pub(crate) Vec<TopBinding>);

#[derive(Debug, Clone)]
pub(crate) struct Context<K, V>(Vec<HashMap<K, V>>);

pub(crate) enum Binding {
    VarType(Option<Type>),
    Type(Type),
}

impl Type {
    pub fn pack(self) -> TyRef {
        Rc::new(self)
    }

    fn map_aux<F1, F2>(&self, c: isize, mut f1: F1, mut f2: F2) -> Self
    where
        F1: FnMut(isize, isize) -> Self,
        F2: FnMut(isize, &str) -> Self,
    {
        use Type::*;
        match self {
            Unit | Str | Integer | Bool => self.clone(),
            Var(x) => f1(c, *x),
            Named(x) => f2(c, x.as_str()),
            All(x, y) => All(x.clone(), y.map_aux(c + 1, &mut f1, &mut f2).pack()),
            Arrow(x, y, z) => Arrow(
                x.map_aux(c, &mut f1, &mut f2).pack(),
                y.map_aux(c, &mut f1, &mut f2).pack(),
                z.as_ref().map(|t| t.map_aux(c, &mut f1, &mut f2).pack()),
            ),
            Variant(x) => Variant(
                x.iter()
                    .map(|(s, t)| {
                        (
                            s.clone(),
                            t.iter().map(|t| t.map_aux(c, &mut f1, &mut f2)).collect(),
                        )
                    })
                    .collect(),
            ),
            Tuple(x) => Tuple(x.iter().map(|t| t.map_aux(c, &mut f1, &mut f2)).collect()),
            Ctor(x, y) => Ctor(
                x.clone(),
                y.iter().map(|t| t.map_aux(c, &mut f1, &mut f2)).collect(),
            ),
        }
    }

    fn map_index<F>(&self, c: isize, f: F) -> Self
    where
        F: FnMut(isize, isize) -> Self,
    {
        self.map_aux(c, f, |_, x| Type::Named(x.into()))
    }

    fn map_named<F>(&self, c: isize, f: F) -> Self
    where
        F: FnMut(isize, &str) -> Self,
    {
        self.map_aux(c, |_, x| Type::Var(x), f)
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
}

impl Expr {
    pub fn pack(self) -> ExRef {
        Rc::new(self)
    }
}

impl<K, V> Context<K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Self {
        Context(vec![HashMap::new()])
    }

    pub fn insert(&mut self, key: K, value: V)
    where
        K: Hash + Eq,
    {
        let last = self.0.last_mut().unwrap();
        last.insert(key, value);
    }

    pub fn lookup<Q: ?Sized>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for level_ctx in self.0.iter().rev() {
            if let Some(v) = level_ctx.get(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn entry(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn exit(&mut self) {
        self.0.pop().unwrap();
    }
}

impl Context<String, Binding> {
    fn find_type(&self, name: &str) -> Result<&Type, Error> {
        if let Some(Binding::Type(x)) = self.lookup(name) {
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

    pub fn type_eq(&self, t1: &Type, t2: &Type) -> bool {
        let t1 = self.simplify_type(t1);
        let t2 = self.simplify_type(t2);
        use Type::*;
        match (t1, t2) {
            (Unit, Unit) | (Bool, Bool) | (Integer, Integer) | (Str, Str) => true,
            (Arrow(t1, t2, None), Arrow(t4, t5, None)) => {
                self.type_eq(t1, t4) && self.type_eq(t2, t5)
            }
            (Arrow(t1, t2, Some(t3)), Arrow(t4, t5, Some(t6))) => {
                self.type_eq(t1, t4) && self.type_eq(t2, t5) && self.type_eq(t3, t6)
            }
            _ => todo!(),
        }
    }
}
