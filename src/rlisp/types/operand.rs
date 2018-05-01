use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Add,
    Sub,
    Mul,
    Div,
    Cat,
    If,
    Scope,
    And,
    Or,
    Eq,
    Not,
    Def,
    Let,
    Func(String),
    List,
}

impl Operand {
    pub fn from_str(s: &str) -> Option<Operand> {
        match s {
            "+" => Some(Operand::Add),
            "-" => Some(Operand::Sub),
            "*" => Some(Operand::Mul),
            "/" => Some(Operand::Div),
            "cat" => Some(Operand::Cat),
            "if" => Some(Operand::If),
            "&" => Some(Operand::And),
            "|" => Some(Operand::Or),
            "=" => Some(Operand::Eq),
            "!" => Some(Operand::Not),
            "def" => Some(Operand::Def),
            "let" => Some(Operand::Let),
            "list" => Some(Operand::List),
            _ => None,
        }
    }
}
impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token = match *self {
            Operand::Add => "+",
            Operand::Sub => "-",
            Operand::Mul => "*",
            Operand::Div => "/",
            Operand::Cat => "cat",
            Operand::If => "if",
            Operand::And => "&",
            Operand::Or => "|",
            Operand::Eq => "=",
            Operand::Not => "!",
            Operand::Def => "def",
            Operand::Scope => "\n",
            Operand::Let => "let",
            Operand::Func(ref token) => token,
            Operand::List => "list",
        };
        write!(f, "{}", token)
    }
}
