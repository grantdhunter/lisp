mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::Eval;

fn main() {
    let tokens = "(+ (if (& true false) 6 5) 1)".to_string().tokenize().unwrap();
    let ast = tokens.parse();
    let result = ast.eval();

    println!("{}", result);
}
