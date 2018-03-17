use tokenizer::Token;
use tokenizer::Operand;
use tokenizer::Atom;
use std::iter::Iterator;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    operand: Option<Operand>,
    args: Box<Vec<Box<Expression>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Atom),
    Expr(Expr),
}

pub trait Parser {
    fn parse(&self) -> Option<Box<Expr>>;
}

impl Parser for Vec<Token> {
    fn parse(&self) -> Option<Box<Expr>> {
        let mut it = self.iter().peekable();
        let expr = Expr {
            operand: Some(Operand::Prg),
            args: Box::new(vec![]),
        };
        parse_expr(&mut it, Box::new(Expression::Expr(expr)))
    }
}

fn parse_expr<'a, It>(it: &'a mut Peekable<It>, stack: Box<Expression>) -> Option<Box<Expr>>
where
    It: Iterator<Item = &'a Token>,
{
    match it.next() {
        Some(ref t) => match *t {
            &Token::OpenBracket => {
                let expr = Expr {
                    operand: None,
                    args: Box::new(vec![]),
                };
                if let Some(r) = parse_expr(it, Box::new(Expression::Expr(expr))) {
                    if let Expression::Expr(mut e) = *stack {
                        e.args.push(Box::new(Expression::Expr(*r)));
                        return Some(Box::new(e));
                    }
                }
                None
            }
            &Token::CloseBracket => {
                if let Expression::Expr(mut e) = *stack {
                    return Some(Box::new(e));
                }
                None
            }
            &Token::Atom(ref a) => {
                if let Expression::Expr(mut e) = *stack {
                    e.args.push(Box::new(Expression::Atom(a.clone())));
                    return parse_expr(it, Box::new(Expression::Expr(e)));
                }
                None
            }
            &Token::Operand(ref o) => {
                if let Expression::Expr(mut e) = *stack {
                    e.operand = Some(o.clone());
                    return parse_expr(it, Box::new(Expression::Expr(e)));
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
            args: Box::new(vec![
                Box::new(Expression::Atom(Atom::Integer(1))),
                Box::new(Expression::Atom(Atom::Integer(2))),
            ]),
        });
        assert_eq!(
            *result,
            Expr {
                operand: Some(Operand::Prg),
                args: Box::new(vec![Box::new(add)]),
            }
        )
    } else {
        assert_eq!(true, false)
    }
}