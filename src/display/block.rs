use std::fmt::Display;

use crate::display::repeat::Repeat;

pub struct Block<'a>(pub &'a str, pub &'a str);

impl Display for Block<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let padding = 4;
        let full = self.1.lines().map(|l| l.len()).max().unwrap_or(16) as u32;
        let rest = full - padding - self.0.len() as u32 - 2;
        let maybe_ln = if self.1.as_bytes().last().is_none_or(|c| *c == b'\n') {
            ""
        } else {
            "\n"
        };
        write!(
            f,
            "\n{} {} {}\n{}{}{}",
            Repeat('-', padding),
            self.0,
            Repeat('-', rest),
            self.1,
            maybe_ln,
            Repeat('-', full)
        )
    }
}
