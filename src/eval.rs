use parser::Expr;
use parser::Expression;


struct ExpressionIterator {
    next_expr: Expr,
    current_expr: Expr
}


impl ExpressionIterator {
    fn new(expr: Expression)-> ExpressionIterator {
        let mut iter =  ExpressionIterator {
            next_expr: 
            current_expr: expr
        }

        
    }
}

impl IntoIterator for Expression {
    type Item = Expression;
    type IntoIter ExpressionIterator;

    fn into_iter(self) -> ExpressionIterator {
        ExpressionIterator::new(self)
    }
}



pub trait Eval {
    fn eval(&self) -> Expression;
}

impl Eval for Expression {
    fn eval(&self) -> Expression {
        
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
}
