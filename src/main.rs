use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::{Eval, Scope};

fn main() {
    let tokens = r#"
(let x (+ 1 2))
(def baz (foo bar) (
  (+ foo bar x)
))

(baz 1 2)

"#.to_string()
        .tokenize()
        .unwrap();

    let ast = tokens.parse();

    let scope = Rc::new(RefCell::new(Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    }));

    let result = ast.eval(scope.clone());

    println!("{}", result);
}
