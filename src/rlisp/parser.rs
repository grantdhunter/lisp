use std::iter::Iterator;

use super::types::{Expr, Expression, Operand, Token};

#[derive(Debug)]
struct ParserOptions {
    in_def: bool,
}

impl ParserOptions {
    fn new() -> ParserOptions {
        ParserOptions { in_def: false }
    }
}

pub trait Parser {
    fn parse(&self) -> Box<Expr>;
}

impl Parser for Vec<Token> {
    fn parse(&self) -> Box<Expr> {
        let mut it = self.iter();
        let expr = Box::new(Expr {
            operand: Some(Operand::Scope),
            args: vec![],
            parent: None,
        });

        let mut options = ParserOptions::new();
        parse_expr(&mut it, expr, &mut options)
    }
}

fn parse_expr<'a, It>(
    it: &'a mut It,
    mut stack: Box<Expr>,
    options: &mut ParserOptions,
) -> Box<Expr>
where
    It: Iterator<Item = &'a Token>,
{
    let t = match it.next() {
        Some(t) => t,
        None => return stack,
    };

    match *t {
        Token::OpenBracket => {
            let expr = Box::new(Expr {
                operand: None,
                args: vec![],
                parent: Some(stack),
            });
            parse_expr(it, expr, options)
        }
        Token::CloseBracket => {
            options.in_def = false;
            if stack.operand.is_none() {
                stack.operand = Some(Operand::Scope)
            }

            if let Some(mut s) = stack.parent.take() {
                s.args.push(Box::new(Expression::Expr(*stack)));
                return parse_expr(it, s, options);
            }
            stack
        }
        Token::Atom(ref a) => {
            if stack.operand.is_none() && options.in_def {
                stack.operand = Some(Operand::List);
            }

            if stack.operand.is_none() {
                stack.operand = Some(Operand::Func(a.to_string()))
            } else {
                stack.args.push(Box::new(Expression::Atom(a.clone())));
            }
            parse_expr(it, stack, options)
        }
        Token::Operand(ref o) => {
            if *o == Operand::Def {
                options.in_def = true;
            }

            stack.operand = Some(o.clone());
            parse_expr(it, stack, options)
        }
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

    let result = tokens.parse();
    let add = Expression::Expr(Expr {
        operand: Some(Operand::Add),
        args: vec![
            Box::new(Expression::Atom(Atom::Integer(1))),
            Box::new(Expression::Atom(Atom::Integer(2))),
        ],
        parent: None,
    });
    assert_eq!(
        *result,
        Expr {
            operand: Some(Operand::Scope),
            args: vec![Box::new(add)],
            parent: None,
        }
    )
}
