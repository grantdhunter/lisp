use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

extern crate copperline;
use copperline::Copperline;

mod tokenizer;
mod parser;
mod eval;

use tokenizer::Tokenizer;
use parser::Parser;
use eval::{Eval, Scope};

fn main() {
    let scope = Rc::new(RefCell::new(Scope {
        parent: None,
        variable: HashMap::new(),
        funcs: HashMap::new(),
    }));

    let cfg = copperline::Config {
        encoding: copperline::Encoding::Utf8,
        mode: copperline::EditMode::Emacs,
    };

    let mut cl = Copperline::new();

    while let Ok(line) = cl.read_line(">> ", &cfg) {
        match line.as_str() {
            "exit" | "quit" | "q" => break,
            "ps" | "printScope" => println!("{:#?}", scope),
            _ => {}
        };

        line.tokenize()
            .map(|s| s.parse())
            .map(|a| a.eval(scope.clone()))
            .map(|r| {
                cl.add_history(line);
                println!("{}", r);
            })
            .unwrap_or_else(|e| println!("{}", e));
    }
}
