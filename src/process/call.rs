use std::{ffi::OsStr, fmt::Display, process::Command};

use crate::{
    die::Mortal,
    display::{
        Block,
        colors::{Red, Reset},
    },
    process::Error,
};

pub fn call<'a>(path: &'a str, args: &'a [impl AsRef<OsStr>]) -> Result<(), Fail<'a>> {
    let output = Command::new(path)
        .args(args)
        .output()
        .or_die_with(|e| Error(path, e));
    if !output.status.success() {
        Err(Fail(path, output.stdout, output.stderr))
    } else {
        Ok(())
    }
}

pub struct Fail<'a>(&'a str, Vec<u8>, Vec<u8>);

impl Display for Fail<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stdout = str::from_utf8(&self.1).unwrap();
        let stderr = str::from_utf8(&self.2).unwrap();
        write!(
            f,
            "{Red}! error running {Reset}`{}`:{}{}",
            self.0,
            Block("stdout", stdout),
            Block("stderr", stderr)
        )
    }
}
