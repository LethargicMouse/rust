use std::{cmp::Ordering, fmt::Display};

use crate::{
    display::colors::{Blue, Red, Reset},
    link::{lex::Lexeme, parse::Parse},
    location::Location,
};

impl<'a> Parse<'a> {
    pub fn error(self) -> Error<'a> {
        let help = self.help();
        let location = self.tokens[self.err_cursor].location;
        let mut msgs = self.msgs;
        msgs.sort();
        msgs.dedup();
        Error {
            location,
            help,
            msgs,
        }
    }

    fn help(&self) -> Option<Help<'a>> {
        if self.tokens[self.err_cursor].lexeme == Lexeme::Else
            && self.err_cursor != 0
            && self.tokens[self.err_cursor - 1].lexeme == Lexeme::Semicolon
        {
            return Some(Help::SemicolonBeforeElse(
                self.tokens[self.err_cursor - 1].location,
            ));
        }
        None
    }

    pub fn fail<T>(&mut self, s: &'static str) -> Result<T, Fail> {
        match self.cursor.cmp(&self.err_cursor) {
            Ordering::Less => {}
            Ordering::Equal => self.msgs.push(s),
            Ordering::Greater => {
                self.err_cursor = self.cursor;
                self.msgs.clear();
                self.msgs.push(s)
            }
        }
        Err(Fail)
    }
}

pub struct Fail;

pub struct Error<'a> {
    location: Location<'a>,
    msgs: Vec<&'static str>,
    help: Option<Help<'a>>,
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{Red}! error parsing {}\n{Red}--! expected:{Blue}",
            self.location
        )?;
        for msg in &self.msgs {
            write!(f, "\n    - {Reset}{msg}{Blue}")?;
        }
        match &self.help {
            Some(help) => write!(f, "\n--@ {help}{Reset}"),
            None => Ok(()),
        }
    }
}

enum Help<'a> {
    SemicolonBeforeElse(Location<'a>),
}

impl Display for Help<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Help::SemicolonBeforeElse(location) => {
                write!(
                    f,
                    "try removing {Reset}`;`{Blue} before {Reset}`else`{Blue} in {location}"
                )
            }
        }
    }
}
