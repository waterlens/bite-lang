use super::*;

impl Type {
    pub fn pack(self) -> TyRef {
        P(self)
    }

    fn map_aux<F1, F2>(self, c: isize, f1: F1, f2: F2) -> Self
    where
        F1: Fn(isize, isize) -> Self + Clone,
        F2: Fn(isize, &str) -> Self + Clone,
    {
        use Type::*;
        match self {
            Unit | Str | Integer | Bool => self,
            Var(x) => f1(c, x),
            Named(x) => f2(c, x.as_str()),
            All(x, y) => All(x, y.map(|y| y.map_aux(c + 1, f1, f2))),
            Arrow(x, y, z) => Arrow(
                x.into_iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
                y.map(|y| y.map_aux(c, f1.clone(), f2.clone())),
                z.map(|t| t.map(|t| t.map_aux(c, f1.clone(), f2.clone()))),
            ),
            Variant(x) => Variant(
                x.into_iter()
                    .map(|(s, t)| {
                        (
                            s.clone(),
                            t.into_iter()
                                .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                                .collect(),
                        )
                    })
                    .collect(),
            ),
            Tuple(x) => Tuple(
                x.into_iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
            ),
            Ctor(x, y) => Ctor(
                x,
                y.into_iter()
                    .map(|t| t.map_aux(c, f1.clone(), f2.clone()))
                    .collect(),
            ),
        }
    }

    fn map_index<F>(self, c: isize, f: F) -> Self
    where
        F: Fn(isize, isize) -> Self + Clone,
    {
        self.map_aux(c, f, |_, x| Type::Named(x.into()))
    }

    fn map_named<F>(self, c: isize, f: F) -> Self
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
            Type::All(x, y) => {
                let y = y
                    .map(|y| y.var_close(x.as_str()))
                    .map(|y| y.to_locally_nameless());
                Type::All(x, y)
            }
            Type::Arrow(x, y, z) => Type::Arrow(
                x.into_iter().map(|x| x.to_locally_nameless()).collect(),
                y.map(|y| y.to_locally_nameless()),
                z.map(|z| z.map(|z| z.to_locally_nameless())),
            ),
            Type::Variant(xs) => Type::Variant(
                xs.into_iter()
                    .map(|(x, y)| (x, y.into_iter().map(|y| y.to_locally_nameless()).collect()))
                    .collect(),
            ),
            Type::Tuple(xs) => {
                Type::Tuple(xs.into_iter().map(|x| x.to_locally_nameless()).collect())
            }
            Type::Ctor(x, ys) => {
                Type::Ctor(x, ys.into_iter().map(|y| y.to_locally_nameless()).collect())
            }
        }
    }

    fn shift_above(self, d: isize, c: isize) -> Self {
        self.map_index(c, |c, x| {
            if x >= c {
                Type::Var(x + d)
            } else {
                Type::Var(x)
            }
        })
    }

    fn shift(self, d: isize) -> Self {
        self.shift_above(d, 0)
    }

    fn subst_n(self, n: isize, ty: &Type) -> Self {
        self.map_index(n, |c, x| {
            if c == x {
                ty.clone().shift(c)
            } else {
                Type::Var(x)
            }
        })
    }

    pub fn subst(self, ty: &Type) -> Self {
        self.subst_n(0, &ty.clone().shift(1)).shift(-1)
    }

    fn open_n(self, n: isize, ty: Type) -> Self {
        self.map_index(n, |c, x| if c == x { ty.clone() } else { Type::Var(x) })
    }

    pub fn open(self, ty: Type) -> Self {
        self.open_n(0, ty)
    }

    fn var_close_n(self, n: isize, name: &str, x: isize) -> Self {
        self.map_named(n, |c, s| {
            if s == name {
                Type::Var(x + c - n)
            } else {
                Type::Named(s.into())
            }
        })
    }

    pub fn var_close(self, name: &str) -> Self {
        self.var_close_n(0, name, 0)
    }

    pub fn free_names(self) -> IndexSet<String> {
        let ls = RefCell::new(IndexSet::new());
        self.map_named(0, |_, s| {
            ls.borrow_mut().insert(s.into());
            Type::Named(s.into())
        });
        ls.take()
    }

    fn remove_quantifiers(self) -> (Vec<String>, Type) {
        let mut v = vec![];
        let mut ty = self;
        while let Type::All(x, inner) = ty {
            v.push(x.clone());
            ty = inner.into_inner();
        }
        (v, ty)
    }

    pub fn normalize(self) -> Type {
        let (names, inner) = self.remove_quantifiers();
        let free = RefCell::new(IndexSet::new());
        let inner = inner.map_index(0, |c, x| {
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
