use crate::link::{
    lex::Lexeme,
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

    pub fn sep<T>(&mut self, f: Parser<'a, T>) -> Vec<T> {
        let mut res = Vec::new();
        if let Some(t) = self.maybe(f) {
            res.push(t);
        }
        while let Some(t) = self.maybe(|p| {
            p.expect(Lexeme::Comma)?;
            f(p)
        }) {
            res.push(t);
        }
        res
    }
}
