use crate::link::parse::{Parse, error::Fail, parsers::Parser};

impl<'a> Parse<'a> {
    pub fn either<T>(&mut self, fs: &[Parser<'a, T>]) -> Result<T, Fail> {
        for f in fs {
            if let Some(res) = self.maybe(*f) {
                return Ok(res);
            }
        }
        Err(Fail)
    }

    pub fn maybe<T>(&mut self, f: Parser<'a, T>) -> Option<T> {
        let before = self.cursor;
        f(self).inspect_err(|_| self.cursor = before).ok()
    }
}
