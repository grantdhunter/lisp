extern crate copperline;

use copperline::Copperline;

mod rlisp;

use rlisp::{Eval, Parser, Scope, Tokenizer};

fn main() {
    let cfg = copperline::Config {
        encoding: copperline::Encoding::Utf8,
        mode: copperline::EditMode::Emacs,
    };

    let mut cl = Copperline::new();

    while let Ok(line) = cl.read_line(">> ", &cfg) {
        match line.as_str() {
            "exit" | "quit" | "q" => break,
            _ => {}
        };

        line.tokenize()
            .map(|s| s.parse())
            .map(|a| a.eval(Scope::new().boxup()))
            .map(|r| {
                cl.add_history(line);
                println!("{}", r);
            })
            .unwrap_or_else(|e| println!("{}", e));
    }
}
