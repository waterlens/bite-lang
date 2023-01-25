use super::*;

#[macro_export]
macro_rules! subst {
    ($var:expr, $ty:expr) => {
        Constraint::new(var, ty)
    };
}
#[macro_export]
macro_rules! substs {
    [$($var:expr => $ty:expr);*] => {
        [$((constr!($var, $ty))),*].to_vec()
    };
}

impl Subst {
    fn new(var: String, ty: Type) -> Self {
        Subst(var, P(ty))
    }
}
