use tokenizer::Token;
use tokenizer::Operand;
use tokenizer::Atom;
use std::iter::Iterator;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub operand: Option<Operand>,
    pub args: Vec<Box<Expression>>,
    pub parent: Option<Box<Expr>>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut disp;
        let mut end = ")";

        if let Some(ref op) = self.operand {
            disp = format!("({}", op);
            if op == &Operand::Scope {
                end = "\n)"
            }
        } else {
            disp = "(wat!".to_string();
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

        parse_expr(&mut it, expr)
    }
}

fn parse_expr<'a, It>(it: &'a mut It, mut stack: Box<Expr>) -> Box<Expr>
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
            parse_expr(it, expr)
        }
        Token::CloseBracket => {
            if let Some(mut s) = stack.parent.take() {
                s.args.push(Box::new(Expression::Expr(*stack)));
                return parse_expr(it, s);
            }
            stack
        }
        Token::Atom(ref a) => {
            stack.args.push(Box::new(Expression::Atom(a.clone())));
            parse_expr(it, stack)
        }
        Token::Operand(ref o) => {
            stack.operand = Some(o.clone());
            parse_expr(it, stack)
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
