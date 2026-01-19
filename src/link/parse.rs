mod combinators;
mod error;
mod parsers;

use crate::{
    die::Mortal,
    link::{
        ast::Ast,
        lex::Token,
        parse::{error::Fail, parsers::Parser},
    },
};

pub fn parse(tokens: Vec<Token>) -> Ast {
    Parse::new(tokens).run(Parse::ast)
}

struct Parse<'a> {
    tokens: Vec<Token<'a>>,
    cursor: usize,
    err_cursor: usize,
    msgs: Vec<&'a str>,
}

impl<'a> Parse<'a> {
    const RESERVED: &'static [&'static str] = &["do", "new", "return"];

    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            tokens,
            cursor: 0,
            err_cursor: 0,
            msgs: Vec::new(),
        }
    }

    fn run<T>(mut self, f: Parser<'a, T>) -> T {
        f(&mut self).or_die_with(|_| self.error())
    }
}
