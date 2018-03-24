mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::Eval;

fn main() {
    println!("========= Eval 2=========\n");
    println!("(+ 1 (* 2 (- 3 1)) 4 (- 9 3) (/ 4 2))");
    let tokens = "(+ 1 (* 2 (- 3 1)) 4 (- 9 3) (/ 4 2))"
        .to_string()
        .tokenize()
        .unwrap();
    println!("{:?}", tokens);
    let ast = tokens.parse();
    println!("{}", ast);
    let result = ast.eval();

    println!("{}", result);
}
