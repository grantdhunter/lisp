use tokenizer::Token;
use tokenizer::Operand;
use tokenizer::Atom;
use std::iter::Iterator;
use std::iter::Peekable;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub operand: Option<Operand>,
    pub args: Vec<Box<Expression>>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut disp;
        let mut end = ")";

        if let Some(ref op) = self.operand {
            disp = format!("({}", op);
            if op == &Operand::Prg {
                end = "\n)"
            }
        } else {
            disp = format!("(wat!");
        }

        for e in &self.args {
            disp.push_str(&format!(" {}", e));
        }
        return write!(f, "{}{}", disp, end);
    }
}

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

pub trait Parser {
    fn parse(&self) -> Option<Box<Expr>>;
}

impl Parser for Vec<Token> {
    fn parse(&self) -> Option<Box<Expr>> {
        let mut it = self.iter().peekable();
        let expr = Expr {
            operand: Some(Operand::Prg),
            args: vec![],
        };
        parse_expr(&mut it, Expression::Expr(expr))
    }
}

fn parse_expr<'a, It>(it: &'a mut Peekable<It>, stack: Expression) -> Option<Box<Expr>>
where
    It: Iterator<Item = &'a Token>,
{
    match it.next() {
        Some(t) => match *t {
            Token::OpenBracket => {
                let expr = Expr {
                    operand: None,
                    args: vec![],
                };
                if let Some(r) = parse_expr(it, Expression::Expr(expr)) {
                    if let Expression::Expr(mut e) = stack {
                        e.args.push(Box::new(Expression::Expr(*r)));
                        return Some(Box::new(e));
                    }
                }
                None
            }
            Token::CloseBracket => {
                if let Expression::Expr(mut e) = stack {
                    return Some(Box::new(e));
                }
                None
            }
            Token::Atom(ref a) => {
                if let Expression::Expr(mut e) = stack {
                    e.args.push(Box::new(Expression::Atom(a.clone())));
                    return parse_expr(it, Expression::Expr(e));
                }
                None
            }
            Token::Operand(ref o) => {
                if let Expression::Expr(mut e) = stack {
                    e.operand = Some(o.clone());
                    return parse_expr(it, Expression::Expr(e));
                }
                None
            }
        },
        None => None,
    }
}

#[test]
fn test_simple_parse() {
    let tokens = vec![
        Token::OpenBracket,
        Token::Operand(Operand::Add),
        Token::Atom(Atom::Integer(1)),
        Token::Atom(Atom::Integer(2)),
        Token::CloseBracket,
    ];

    if let Some(result) = tokens.parse() {
        let add = Expression::Expr(Expr {
            operand: Some(Operand::Add),
            args: vec![
                Box::new(Expression::Atom(Atom::Integer(1))),
                Box::new(Expression::Atom(Atom::Integer(2))),
            ],
        });
        assert_eq!(
            *result,
            Expr {
                operand: Some(Operand::Prg),
                args: vec![Box::new(add)],
            }
        )
    } else {
        assert_eq!(true, false)
    }
}
