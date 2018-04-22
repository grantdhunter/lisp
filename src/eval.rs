use parser::Expr;
use parser::Expression;
use tokenizer::Operand;
use tokenizer::Atom;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub variable: HashMap<String, Expression>,
    pub funcs: HashMap<String, Func>,
}

impl Scope {
    fn get_var(&self, key: &String) -> Option<Expression> {
        if let Some(t) = self.variable.get(key) {
            return Some(t.clone());
        }

        if let Some(ref s) = self.parent {
            return s.borrow().get_var(key);
        }
        None
    }

    fn get_func<'a>(&'a self, key: &'a String) -> Option<Func> {
        if let Some(t) = self.funcs.get(key) {
            return Some(t.clone());
        }

        if let Some(ref p) = self.parent {
            return p.borrow().get_func(key);
        }
        None
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub args: Vec<Atom>,
    pub body: Expr,
}

pub trait Eval {
    fn eval(&self, scope: Rc<RefCell<Scope>>) -> Expression;
}

impl Eval for Expression {
    fn eval(&self, scope: Rc<RefCell<Scope>>) -> Expression {
        match *self {
            Expression::Expr(ref e) => {
                println!("Expr: {}", e);
                e.eval(scope.clone())
            }
            Expression::Atom(Atom::Token(ref t)) => {
                println!("Atom: {}", t);
                if let Some(v) = scope.borrow().get_var(t) {
                    println!("var: {}", v);
                    return v;
                }
                Expression::Atom(Atom::Null)
            }
            _ => {
                println!("other: {}", self);
                self.clone()
            }
        }
    }
}

impl Eval for Expr {
    fn eval(&self, scope: Rc<RefCell<Scope>>) -> Expression {
        if let Some(ref o) = self.operand {
            return o.execute(&self.args, scope.clone());
        }
        println!("eval expr: {:#?}", self);
        Expression::Atom(Atom::Null)
    }
}

impl Operand {
    fn execute(&self, args: &[Box<Expression>], scope: Rc<RefCell<Scope>>) -> Expression {
        match *self {
            Operand::Add => Expression::Atom(Atom::Integer(args.iter().fold(0, |acc, a| {
                if let Expression::Atom(Atom::Integer(i)) = a.eval(scope.clone()) {
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
                                if let Expression::Atom(Atom::Integer(i)) = a.eval(scope.clone()) {
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
                if let Expression::Atom(Atom::Integer(i)) = a.eval(scope.clone()) {
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
                                if let Expression::Atom(Atom::Integer(i)) = a.eval(scope.clone()) {
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
                    if let Expression::Atom(Atom::String(s)) = a.eval(scope.clone()) {
                        acc.push_str(&s);
                    }
                    println!("Default cat");
                    acc
                },
            ))),
            Operand::Scope => args.iter()
                .fold(Expression::Atom(Atom::Null), |_, a| a.eval(scope.clone())),
            Operand::If => {
                if let Some(Expression::Atom(Atom::Bool(b))) =
                    args.first().map(|a| a.eval(scope.clone()))
                {
                    if b {
                        return args.get(1)
                            .map_or(Expression::Atom(Atom::Null), |a| a.eval(scope.clone()));
                    } else {
                        return args.get(2)
                            .map_or(Expression::Atom(Atom::Null), |a| a.eval(scope.clone()));
                    }
                }
                println!("Default if");
                Expression::Atom(Atom::Null)
            }
            Operand::And => Expression::Atom(Atom::Bool(args.iter().fold(true, |acc, a| {
                if let Expression::Atom(Atom::Bool(i)) = a.eval(scope.clone()) {
                    return acc && i;
                }
                println!("Default And");
                false
            }))),
            Operand::Or => Expression::Atom(Atom::Bool(args.iter().fold(false, |acc, a| {
                if let Expression::Atom(Atom::Bool(i)) = a.eval(scope.clone()) {
                    return acc || i;
                }
                println!("Default or");
                false
            }))),
            Operand::Eq => {
                if let (Some(a1), Some(a2)) = (args.first(), args.get(1)) {
                    return Expression::Atom(Atom::Bool(
                        a1.eval(scope.clone()) == a2.eval(scope.clone()),
                    ));
                }
                println!("Default Eq");
                Expression::Atom(Atom::Bool(false))
            }
            Operand::Not => {
                if let Some(a) = args.first() {
                    if let Expression::Atom(Atom::Bool(b)) = a.eval(scope.clone()) {
                        return Expression::Atom(Atom::Bool(!b));
                    }
                }
                Expression::Atom(Atom::Bool(false))
            }
            Operand::Def => {
                if let (Some(name), Some(arguments), Some(body)) =
                    (args.get(0), args.get(1), args.get(2))
                {
                    if let Expression::Expr(ref body) = **body {
                        if let Expression::Expr(ref a) = **arguments {
                            let func = Func {
                                args: a.args
                                    .iter()
                                    .filter_map(|a| match **a {
                                        Expression::Atom(ref a) => Some(a.clone()),
                                        Expression::Expr(_) => None,
                                    })
                                    .collect(),
                                body: body.clone(),
                            };
                            scope.borrow_mut().funcs.insert(name.to_string(), func);
                        }
                    }
                }

                Expression::Atom(Atom::Null)
            }
            Operand::Func(ref name) => {
                println!("user func {}", name);
                println!("{:#?}", scope);

                let params = args.iter().map(|a| a.eval(scope.clone()));
                let s = scope.borrow();
                let f = s.get_func(name);

                if let Some(f) = f {
                    let mut vars = HashMap::new();
                    f.args.iter().zip(params).for_each(|(k, v)| {
                        vars.insert(k.to_string(), v);
                    });

                    let mut local_scope = Rc::new(RefCell::new(Scope {
                        parent: Some(scope.clone()),
                        variable: vars,
                        funcs: HashMap::new(),
                    }));
                    println!("func: {}", f.body);
                    return f.body.eval(local_scope.clone());
                }
                Expression::Atom(Atom::Null)
            }
            Operand::Let => {
                if let (Some(name), Some(exp)) = (args.first(), args.get(1)) {
                    if let Expression::Atom(Atom::Token(ref t)) = *(*name) {
                        let val = exp.eval(scope.clone());
                        scope.borrow_mut().variable.insert(t.clone(), val);
                    }
                }
                Expression::Atom(Atom::Null)
            }
            Operand::List => Expression::Atom(Atom::List(
                args.iter()
                    .map(|a| a.eval(scope.clone()))
                    .filter_map(|a| match a {
                        Expression::Atom(a) => Some(a),
                        Expression::Expr(_) => None,
                    })
                    .collect(),
            )),
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
        operand: Some(Operand::Scope),
        args: vec![Box::new(add)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(sub)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(mul)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(div)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(cat)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(if_true)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(if_false)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

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
        operand: Some(Operand::Scope),
        args: vec![Box::new(if_false)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

    assert_eq!(
        result,
        Expression::Atom(Atom::String("That is False".to_string()))
    )
}

#[test]
fn test_simple_and() {
    let and = Expression::Expr(Expr {
        operand: Some(Operand::And),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(true))),
            Box::new(Expression::Atom(Atom::Bool(false))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(and)],
        parent: None,
    };

    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Bool(false)))
}

#[test]
fn test_simple_or() {
    let or = Expression::Expr(Expr {
        operand: Some(Operand::Or),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(true))),
            Box::new(Expression::Atom(Atom::Bool(false))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(or)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Bool(true)))
}
#[test]
fn test_simple_eq() {
    let eq = Expression::Expr(Expr {
        operand: Some(Operand::Eq),
        args: vec![
            Box::new(Expression::Atom(Atom::Bool(true))),
            Box::new(Expression::Atom(Atom::Bool(false))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(eq)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Bool(false)))
}
#[test]
fn test_simple_not() {
    let not = Expression::Expr(Expr {
        operand: Some(Operand::Not),
        args: vec![Box::new(Expression::Atom(Atom::Bool(false)))],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(not)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Bool(true)))
}
#[test]
fn test_simple_let() {
    let let_st = Expression::Expr(Expr {
        operand: Some(Operand::Let),
        args: vec![
            Box::new(Expression::Atom(Atom::Token("var".to_string()))),
            Box::new(Expression::Atom(Atom::Bool(false))),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(let_st)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Null));
    assert_eq!(
        scope.variable.get("var"),
        Some(&Expression::Atom(Atom::Bool(false)))
    );
}

#[test]
fn test_simple_def() {
    let def = Expression::Expr(Expr {
        operand: Some(Operand::Def),
        args: vec![
            Box::new(Expression::Atom(Atom::Token("baz".to_string()))),
            Box::new(Expression::Atom(Atom::List(vec![
                Atom::Token("foo".to_string()),
                Atom::Token("bar".to_string()),
            ]))),
            Box::new(Expression::Expr(Expr {
                operand: Some(Operand::Add),
                args: vec![
                    Box::new(Expression::Atom(Atom::Token("foo".to_string()))),
                    Box::new(Expression::Atom(Atom::Token("bar".to_string()))),
                ],
                parent: None,
            })),
        ],
        parent: None,
    });

    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(def)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Null));
    assert_eq!(
        scope.funcs.get("baz"),
        Some(&Func {
            args: vec![
                Atom::Token("foo".to_string()),
                Atom::Token("bar".to_string()),
            ],
            body: Expr {
                operand: Some(Operand::Add),
                args: vec![
                    Box::new(Expression::Atom(Atom::Token("foo".to_string()))),
                    Box::new(Expression::Atom(Atom::Token("bar".to_string()))),
                ],
                parent: None,
            },
        })
    );
}

#[test]
fn test_simple_user_func_call() {
    let def = Expression::Expr(Expr {
        operand: Some(Operand::Def),
        args: vec![
            Box::new(Expression::Atom(Atom::Token("baz".to_string()))),
            Box::new(Expression::Atom(Atom::List(vec![
                Atom::Token("foo".to_string()),
                Atom::Token("bar".to_string()),
            ]))),
            Box::new(Expression::Expr(Expr {
                operand: Some(Operand::Add),
                args: vec![
                    Box::new(Expression::Atom(Atom::Token("foo".to_string()))),
                    Box::new(Expression::Atom(Atom::Token("bar".to_string()))),
                ],
                parent: None,
            })),
        ],
        parent: None,
    });

    let call = Expression::Atom(Atom::Token("baz".to_string()));
    let ast = Expr {
        operand: Some(Operand::Scope),
        args: vec![Box::new(def), Box::new(call)],
        parent: None,
    };
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);
    assert_eq!(result, Expression::Atom(Atom::Null));
    assert_eq!(
        scope.funcs.get("baz"),
        Some(&Func {
            args: vec![
                Atom::Token("foo".to_string()),
                Atom::Token("bar".to_string()),
            ],
            body: Expr {
                operand: Some(Operand::Add),
                args: vec![
                    Box::new(Expression::Atom(Atom::Token("foo".to_string()))),
                    Box::new(Expression::Atom(Atom::Token("bar".to_string()))),
                ],
                parent: None,
            },
        })
    );
}
