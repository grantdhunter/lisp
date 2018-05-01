use std::fmt;

use super::{Atom, Expr};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Expr(Expr),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Atom(ref a) => write!(f, "{}", a),
            Expression::Expr(ref e) => write!(f, "{}", e),
        }
    }
}
