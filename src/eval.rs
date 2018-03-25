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
                println!("Default Add");
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
                                println!("Default Sub");
                                0
                            },
                        )));
                    }
                }
                println!("Default Sub");
                Expression::Atom(Atom::Integer(0))
            }
            Operand::Mul => Expression::Atom(Atom::Integer(args.iter().fold(1, |acc, a| {
                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                    return acc * i;
                }
                println!("Default mul");
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
                                println!("Default div");
                                0
                            },
                        )));
                    }
                }
                println!("Default div");
                Expression::Atom(Atom::Integer(0))
            }
            Operand::Cat => Expression::Atom(Atom::String(args.iter().fold(
                String::new(),
                |mut acc, a| {
                    if let Expression::Atom(Atom::String(s)) = a.eval() {
                        acc.push_str(&s);
                    }
                    println!("Default cat");
                    acc
                },
            ))),
            Operand::Prg => args[0].eval(),
            Operand::If => {
                if let Some(Expression::Atom(Atom::Bool(b))) = args.first().map(|a| a.eval()) {
                    if b {
                        return args.get(1)
                            .map_or(Expression::Atom(Atom::Null), |a| a.eval());
                    } else {
                        return args.get(2)
                            .map_or(Expression::Atom(Atom::Null), |a| a.eval());
                    }
                }
                println!("Default if");
                Expression::Atom(Atom::Null)
            }
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
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(add)],
        parent: None,
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
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(sub)],
        parent: None,
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
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(mul)],
        parent: None,
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
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(div)],
        parent: None,
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(2)))
}

#[test]
fn test_simple_cat() {
    let cat = Expression::Expr(Expr {
        operand: Some(Operand::Cat),
        args: vec![
            Box::new(Expression::Atom(Atom::String("foo".to_string()))),
            Box::new(Expression::Atom(Atom::String("bar".to_string()))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(cat)],
        parent: None,
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::String("foobar".to_string())))
}

#[test]
fn test_simple_if_true() {
    let if_true = Expression::Expr(Expr {
        operand: Some(Operand::If),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(true))),
            Box::new(Expression::Atom(Atom::String("That is True".to_string()))),
            Box::new(Expression::Atom(Atom::String("That is False".to_string()))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(if_true)],
        parent: None,
    };

    let result = ast.eval();

    assert_eq!(
        result,
        Expression::Atom(Atom::String("That is True".to_string()))
    )
}
#[test]
fn test_simple_if_no_else() {
    let if_false = Expression::Expr(Expr {
        operand: Some(Operand::If),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(false))),
            Box::new(Expression::Atom(Atom::String("That is True".to_string()))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(if_false)],
        parent: None,
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Null))
}

#[test]
fn test_simple_if_false() {
    let if_false = Expression::Expr(Expr {
        operand: Some(Operand::If),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(false))),
            Box::new(Expression::Atom(Atom::String("That is True".to_string()))),
            Box::new(Expression::Atom(Atom::String("That is False".to_string()))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: vec![Box::new(if_false)],
        parent: None,
    };

    let result = ast.eval();

    assert_eq!(
        result,
        Expression::Atom(Atom::String("That is False".to_string()))
    )
}
