mod call;
pub use call::call;

use std::{
    ffi::OsStr,
    fmt::Display,
    io,
    process::{Command, exit},
};

use crate::die::Mortal;

struct Error<'a>(&'a str, io::Error);

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "! error trying to run `{}`: {}", self.0, self.1)
    }
}

pub fn run(path: &str, args: impl IntoIterator<Item = impl AsRef<OsStr>>) {
    let status = Command::new(path)
        .args(args)
        .status()
        .or_die_with(|e| Error(path, e));
    if !status.success() {
        exit(status.code().unwrap_or(1))
    }
}
