use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;
use std::fmt;

//thanks to https://keepcalmandlearnrust.com/2016/08/iterator-and-peekable/

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Add,
    Sub,
    Mul,
    Div,
    Cat,
    If,
    Prg,
}

impl Operand {
    pub fn from_str(s: &str) -> Option<Operand> {
        match s {
            "+" => Some(Operand::Add),
            "-" => Some(Operand::Sub),
            "*" => Some(Operand::Mul),
            "/" => Some(Operand::Div),
            "cat" => Some(Operand::Cat),
            "if" => Some(Operand::If)
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
            Operand::Prg => "\n",
            Operand::Cat => "cat",
            Operand::if => "If",
        };
        write!(f, "{}", token)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    String(String),
    Token(String),
    Integer(i64),
    Null,
}
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let token = match *self {
            Atom::Integer(ref i) => i.to_string(),
            Atom::String(ref s) => s.to_string(),
            Atom::Token(ref t) => t.to_string(),
            Atom::Null => "null".to_string(),
        };
        write!(f, "{}", token)
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
    Atom(Atom),
    Operand(Operand),
}

pub trait Tokenizer {
    fn tokenize(&self) -> Result<Vec<Token>, &'static str>;
}

impl Tokenizer for String {
    fn tokenize(&self) -> Result<Vec<Token>, &'static str> {
        let mut it = self.chars().peekable();
        let mut tokens: Vec<Token> = vec![];

        while let Some(&c) = it.peek() {
            match c {
                '(' => {
                    it.next();
                    tokens.push(Token::OpenBracket);
                }
                ')' => {
                    it.next();
                    tokens.push(Token::CloseBracket);
                }

                '0'...'9' => {
                    let num = consume(&mut it, |b| b.is_numeric()).parse::<i64>().unwrap();
                    tokens.push(Token::Atom(Atom::Integer(num)));
                }
                '"' | '\'' => {
                    it.next();
                    tokens.push(Token::Atom(Atom::String(consume_until(&mut it, |b| {
                        b == '"' || b == '\''
                    }))));
                }
                c if c.is_alphanumeric() => tokens
                    .push(Token::Atom(Atom::Token(consume(&mut it, |b| {
                        b.is_alphanumeric()
                    })))),
                '+' | '-' | '*' | '/' => {
                    it.next();
                    tokens.push(Token::Operand(Operand::from_str(&c.to_string()).unwrap()));
                }
                _ => {
                    it.next();
                }
            }
        }
        Ok(tokens)
    }
}

fn consume<F>(it: &mut Peekable<Chars>, cond: F) -> String
where
    F: Fn(char) -> bool,
{
    let mut v: Vec<char> = vec![];
    while let Some(&c) = it.peek() {
        if cond(c) {
            it.next();
            v.push(c);
        } else {
            break;
        }
    }
    v.into_iter().collect()
}
fn consume_until<F>(it: &mut Peekable<Chars>, cond: F) -> String
where
    F: Fn(char) -> bool,
{
    let mut v: Vec<char> = vec![];
    while let Some(&c) = it.peek() {
        if cond(c) {
            it.next();
            break;
        } else {
            if c == '\\' {
                it.next();
            }

            it.next();
            v.push(c);
        }
    }
    v.into_iter().collect()
}

#[test]
fn test_simple_tokenizer() {
    let tokens = "(+ 1 234 \"asd\" qwe \"zxc\n\")"
        .to_string()
        .tokenize()
        .unwrap();
    println!("{:?}", tokens);
    assert_eq!(tokens[0], Token::OpenBracket);
    assert_eq!(tokens[1], Token::Operand(Operand::Add));
    assert_eq!(tokens[2], Token::Atom(Atom::Integer(1)));
    assert_eq!(tokens[3], Token::Atom(Atom::Integer(234)));
    assert_eq!(tokens[4], Token::Atom(Atom::String("asd".to_string())));
    assert_eq!(tokens[5], Token::Atom(Atom::Token("qwe".to_string())));
    assert_eq!(tokens[6], Token::Atom(Atom::String("zxc\n".to_string())));
    assert_eq!(tokens[7], Token::CloseBracket);
    assert_eq!(tokens.len(), 8);
}
