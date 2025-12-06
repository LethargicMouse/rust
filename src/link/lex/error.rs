use std::fmt::Display;

use crate::location::Location;

pub struct Error<'a>(pub Location<'a>);

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "! error lexing {}\n--! unexpected token", self.0)
    }
}

pub struct Unclosed<'a>(pub Location<'a>);

impl Display for Unclosed<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "! error lexing {}\n--! unclosed string literal", self.0)
    }
}
