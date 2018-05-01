use super::{Atom, Expr};

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub args: Vec<Atom>,
    pub body: Expr,
}
