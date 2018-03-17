mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;

use parser::Parser;

fn main() {
    let tokens = "(+ 1 1)".to_string().tokenize().unwrap();
    println!("{:?}", tokens);
    let ast = tokens.parse();
    println!("{:?}", ast);


     let tokens = "(+ (- 4 5) 1 1)".to_string().tokenize().unwrap();
    println!("{:?}", tokens);
    let ast = tokens.parse();
    println!("{:?}", ast);

}
