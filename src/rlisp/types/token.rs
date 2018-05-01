use super::{Atom, Operand};

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
    Atom(Atom),
    Operand(Operand),
}
