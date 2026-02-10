mod display;

use std::fmt::{Debug, Display};

use crate::{
    display::colors::Blue,
    location::display::{Line, Underline},
    source::Source,
};

#[derive(Clone, Copy)]
pub struct Location<'a> {
    pub source: &'a Source,
    pub start: Pos,
    pub end: Pos,
}

impl<'a> Location<'a> {
    pub fn combine(self, other: Self) -> Self {
        Self {
            source: self.source,
            start: self.start,
            end: other.end,
        }
    }

    pub fn after(self) -> Self {
        Self {
            start: self.end,
            end: self.end.after(b' '),
            ..self
        }
    }
}

impl Display for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{Blue}{} at {}:\n     |{}",
            self.source.name,
            self.start,
            Line(self.start.line, self.source)
        )?;
        if self.start.line == self.end.line {
            write!(f, "{}", Underline(self.start.symbol, self.end.symbol))?;
            return Ok(());
        }
        write!(
            f,
            "{}",
            Underline(
                self.start.symbol,
                self.source.get_line(self.start.line as usize - 1).len() as u32 + 1
            )
        )?;
        for line in self.start.line + 1..=self.end.line {
            write!(f, "{}", Line(line, self.source))?;
        }
        write!(f, "{}", Underline(1, self.end.symbol))
    }
}

impl Debug for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Pos {
    pub line: u32,
    pub symbol: u32,
}

impl Pos {
    pub fn after(mut self, c: u8) -> Self {
        if c == b'\n' {
            self.line += 1;
            self.symbol = 1;
        } else {
            self.symbol += 1;
        }
        self
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.symbol)
    }
}

pub const BEGIN: Pos = Pos { line: 1, symbol: 1 };
