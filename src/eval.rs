use parser::Expr;
use parser::Expression;
use tokenizer::Operand;
use tokenizer::Atom;

pub trait Eval {
    fn eval(&self) -> Expression;
}

impl Eval for Expression {
    fn eval(&self) -> Expression {
        if let &Expression::Expr(ref e) = self {
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
        return Expression::Atom(Atom::Null);
    }
}

impl Operand {
    fn execute(&self, args: &Box<Vec<Box<Expression>>>) -> Expression {
        match self {
            &Operand::Add => Expression::Atom(Atom::Integer(args.iter().fold(0, |acc, a| {
                if let Expression::Atom(Atom::Integer(i)) = a.eval() {
                    return acc + i;
                }
                return 0;
            }))),
            &Operand::Prg => args[0].eval(),
            _ => Expression::Atom(Atom::Null),
        }
    }
}

#[test]
fn test_simple_eval() {
    let add = Expression::Expr(Expr {
        operand: Some(Operand::Add),
        args: Box::new(vec![
            Box::new(Expression::Atom(Atom::Integer(1))),
            Box::new(Expression::Atom(Atom::Integer(2))),
        ]),
    });

    let ast = Expr {
        operand: Some(Operand::Prg),
        args: Box::new(vec![Box::new(add)]),
    };

    let result = ast.eval();

    assert_eq!(result, Expression::Atom(Atom::Integer(3)))
}
