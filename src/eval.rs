use parser::Expr;
use parser::Expression;
use tokenizer::Operand;
use tokenizer::Atom;

pub trait Eval {
    fn eval(&self) -> Expression;
}

impl Eval for Expression {
    fn eval(&self) -> Expression {
        if let Expression::Expr(ref e) = *self {
            return e.eval();
        }

        self.clone()
    }
}

impl Eval for Expr {
    fn eval(&self) -> Expression {
        if let Some(ref o) = self.operand {
            return o.execute(&self.args);
        }
        Expression::Atom(Atom::Null)
    }
}

impl Operand {
    fn execute(&self, args: &[Box<Expression>]) -> Expression {
        match *self {
            Operand::Add => Expression::Atom(Atom::Integer(args.iter().fold(0, |acc, a| {
                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                    return acc + i;
                }
                0
            }))),
            Operand::Sub => {
                if let Some(init) = args.iter().next() {
                    if let Expression::Atom(Atom::Integer(init)) = **init {
                        return Expression::Atom(Atom::Integer(args.iter().skip(1).fold(
                            init,
                            |acc, a| {
                                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                                    return acc - i;
                                }
                                0
                            },
                        )));
                    }
                }
                Expression::Atom(Atom::Integer(0))
            }
            Operand::Mul => Expression::Atom(Atom::Integer(args.iter().fold(1, |acc, a| {
                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                    return acc * i;
                }
                0
            }))),
            Operand::Div => {
                if let Some(init) = args.iter().next() {
                    if let Expression::Atom(Atom::Integer(init)) = **init {
                        return Expression::Atom(Atom::Integer(args.iter().skip(1).fold(
                            init,
                            |acc, a| {
                                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                                    return acc / i;
                                }
                                0
                            },
                        )));
                    }
                }
                Expression::Atom(Atom::Integer(0))
            }
            Operand::Prg => args[0].eval(),
            _ => Expression::Atom(Atom::Null),
        }
    }
}

#[test]
fn test_simple_add() {
    let add = Expression::Expr(Expr {
        operand: Some(Operand::Add),
        args: vec![
            Box::new(Expression::Atom(Atom::Integer(1))),
            Box::new(Expression::Atom(Atom::Integer(2))),
            Box::new(Expression::Atom(Atom::Integer(3))),
        ],
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(add)],
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(6)))
}
#[test]
fn test_simple_sub() {
    let sub = Expression::Expr(Expr {
        operand: Some(Operand::Sub),
        args: vec![
            Box::new(Expression::Atom(Atom::Integer(4))),
            Box::new(Expression::Atom(Atom::Integer(2))),
            Box::new(Expression::Atom(Atom::Integer(1))),
        ],
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(sub)],
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(1)))
}

#[test]
fn test_simple_mul() {
    let mul = Expression::Expr(Expr {
        operand: Some(Operand::Mul),
        args: vec![
            Box::new(Expression::Atom(Atom::Integer(4))),
            Box::new(Expression::Atom(Atom::Integer(2))),
            Box::new(Expression::Atom(Atom::Integer(1))),
        ],
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(mul)],
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(8)))
}

#[test]
fn test_simple_div() {
    let div = Expression::Expr(Expr {
        operand: Some(Operand::Div),
        args: vec![
            Box::new(Expression::Atom(Atom::Integer(4))),
            Box::new(Expression::Atom(Atom::Integer(2))),
            Box::new(Expression::Atom(Atom::Integer(1))),
        ],
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(div)],
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(2)))
}
