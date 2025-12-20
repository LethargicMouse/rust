use std::fmt::Display;

use crate::{
    display::colors::{Blue, Reset},
    source::Source,
};

pub struct Line<'a>(pub u32, pub &'a Source);

impl Display for Line<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\n{Blue}{:4} |{Reset} {}",
            self.0,
            self.1.get_line(self.0 as usize - 1)
        )
    }
}

pub struct Underline(pub u32, pub u32);

impl Display for Underline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n     {Blue}|")?;
        for _ in 0..self.0 {
            write!(f, " ")?;
        }
        for _ in self.0..self.1 {
            write!(f, "`")?;
        }
        write!(f, "{Reset}")
    }
}
