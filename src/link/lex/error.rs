use std::fmt::Display;

use crate::{
    display::colors::{Red, Reset},
    location::Location,
};

pub struct Unclosed<'a>(pub Location<'a>);

impl Display for Unclosed<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{Red}! error lexing {}\n{Red}--! unclosed string literal{Reset}",
            self.0
        )
    }
}
