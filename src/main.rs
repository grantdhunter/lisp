mod tokenizer;
mod executer;

use tokenizer::Tokenizer;

fn main() {
    let tokens = "(+ 1 1)".to_string().tokenize();
    println!("{:?}", tokens);
}
