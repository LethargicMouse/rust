use std::fmt::Display;

pub struct Sep<'a, T>(pub &'static str, pub &'a [T]);

impl<T: Display> Display for Sep<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.1.is_empty() {
            return Ok(());
        }
        write!(f, "{}", self.1[0])?;
        for t in &self.1[1..] {
            write!(f, "{}{t}", self.0)?;
        }
        Ok(())
    }
}
