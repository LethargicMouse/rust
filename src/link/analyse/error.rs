use std::fmt::Display;

use crate::{
    Location,
    display::colors::{Red, Reset},
};

pub struct NotDeclared<'a> {
    pub location: Location<'a>,
    pub kind: &'static str,
    pub name: &'a str,
}

impl Display for NotDeclared<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! {} {Reset}`{}`{Red} is not declared{Reset}",
            self.location, self.kind, self.name
        )
    }
}

pub enum CheckError<'a> {
    ND(NotDeclared<'a>),
}

impl Display for CheckError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{Red}! error checking ")?;
        match self {
            CheckError::ND(not_declared) => write!(f, "{not_declared}"),
        }
    }
}

pub struct Fail;
