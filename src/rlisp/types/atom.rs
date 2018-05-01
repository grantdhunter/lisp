use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    String(String),
    Token(String),
    Integer(i64),
    Bool(bool),
    List(Vec<Atom>),
    Null,
}
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token = match *self {
            Atom::Integer(ref i) => i.to_string(),
            Atom::String(ref s) => s.to_string(),
            Atom::Token(ref t) => t.to_string(),
            Atom::Bool(ref b) => b.to_string(),
            Atom::List(ref l) => format!("{:?}", l),
            Atom::Null => "null".to_string(),
        };
        write!(f, "{}", token)
    }
}
