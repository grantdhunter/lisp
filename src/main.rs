mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::Eval;

fn main() {

    println!("========= Tokenizer =========\n");
    let tokens = "(+ 1 1 (+ 3 4))".to_string().tokenize().unwrap();
    println!("{:?}", tokens);

    println!("========= Parser =========\n");
    let tokens = "(+ (- 4 5) 1 1)".to_string().tokenize().unwrap();
    let ast = tokens.parse().unwrap();
    println!("{:?}", ast);

    println!("========= Eval =========\n");
    let tokens = "(+ 1 1 3)".to_string().tokenize().unwrap();
    let ast = tokens.parse().unwrap();
    let result = ast.eval();
    println!("{:?}", result);
}
