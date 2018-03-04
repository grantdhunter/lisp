use tokenizer::Token;
use std::iter::Peekable;
use std::iter::Iterator;

trait Executer {
    fn execute(&self) -> Result<i32, &'static str>;
}

impl Executer for Vec<Token> {
    fn execute(&self) -> Result<Token, &'static str> {
        let it = self.iter().peekable();
        execute(it)
    }
}

fn execute(it: &mut Peekable<Iterator>) -> Result<Token, &'static str> {
    loop {
        match it.peek() {}
    }

    Ok(Token::Atom(0))
}

#[test]
fn test_simple_execute() {
    let tokens = vec![
        Token::OpenBracket,
        Token::Operand(Operand::ADD),
        Token::Integer(1),
        Token::Integer(2),
        Token::CloseBracket,
    ];

    let result = tokenizer.execute();
    assert_eq!(result, Ok(Token::Interger(3)))
}
