use crate::link::{
    lex::Lexeme::Comma,
    parse::{Parse, error::Fail, parsers::Parser},
};

impl<'a> Parse<'a> {
    pub fn either<T>(&mut self, fs: &[Parser<'a, T>]) -> Result<T, Fail> {
        for f in fs {
            if let Some(res) = self.maybe(*f) {
                return Ok(res);
            }
        }
        Err(Fail)
    }

    pub fn maybe<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, Fail>) -> Option<T> {
        let before = self.cursor;
        f(self).inspect_err(|_| self.cursor = before).ok()
    }

    pub fn many<T>(&mut self, f: Parser<'a, T>) -> Vec<T> {
        let mut res = Vec::new();
        while let Some(t) = self.maybe(f) {
            res.push(t)
        }
        res
    }

    pub fn sep<T>(&mut self, parser: Parser<'a, T>) -> ParseSep<'a, '_, T> {
        ParseSep {
            parse: self,
            parser,
            first: true,
        }
    }
}

pub struct ParseSep<'a, 'b, T> {
    parse: &'b mut Parse<'a>,
    parser: Parser<'a, T>,
    first: bool,
}

impl<T> Iterator for ParseSep<'_, '_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.first {
            self.first = false;
            self.parse.maybe(self.parser)
        } else {
            self.parse.expect(Comma).ok()?;
            self.parse.maybe(self.parser)
        }
    }
}
