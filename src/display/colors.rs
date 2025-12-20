use std::fmt::Display;

pub enum Color {
    Red,
    Blue,
    Reset,
}

pub use Color::*;

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[")?;
        match self {
            Red => write!(f, "1;31"),
            Blue => write!(f, "0;34"),
            Reset => write!(f, "0"),
        }?;
        write!(f, "m")
    }
}
