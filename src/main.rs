use std::collections::HashMap;

mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::{Eval, Scope};

fn main() {
    let tokens = r#"
(+ (if (& true false) 6 5) 1)

"#.to_string()
        .tokenize()
        .unwrap();
    let ast = tokens.parse();
    let mut scope = Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    };

    let result = ast.eval(&mut scope);

    println!("{}", result);
}
