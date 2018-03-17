use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

//thanks to https://keepcalmandlearnrust.com/2016/08/iterator-and-peekable/

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Add,
    Sub,
    Prg,
}

impl Operand {
    pub fn from_str(s: &str) -> Option<Operand> {
        match s {
            "+" => Some(Operand::Add),
            "-" => Some(Operand::Sub),
            _ => None,
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    String(String),
    Integer(i64),
    Boolean(bool),
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

        loop {
            match it.peek() {
                Some(&c) => match c {
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
                        tokens.push(Token::Atom(Atom::Integer(num)))
                    }
                    c if c.is_alphanumeric() => tokens
                        .push(Token::Atom(Atom::String(consume(&mut it, |b| {
                            b.is_alphanumeric()
                        })))),
                    '+' | '-' => {
                        it.next();
                        tokens.push(Token::Operand(Operand::from_str(&c.to_string()).unwrap()));
                    }
                    _ => {
                        it.next();
                    }
                },
                None => break,
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

#[test]
fn test_simple_tokenizer() {
    let tokens = "(+ 1 234)".to_string().tokenize().unwrap();
    assert_eq!(tokens[0], Token::OpenBracket);
    assert_eq!(tokens[1], Token::Operand(Operand::Add));
    assert_eq!(tokens[2], Token::Atom(Atom::Integer(1)));
    assert_eq!(tokens[3], Token::Atom(Atom::Integer(234)));
    assert_eq!(tokens[4], Token::CloseBracket);
    assert_eq!(tokens.len(), 5);
}
