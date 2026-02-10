use std::{cmp::Ordering, fmt::Display};

use crate::{
    display::colors::{Blue, Red, Reset},
    link::{lex::Lexeme, parse::Parse},
    location::Location,
};

impl<'a> Parse<'a> {
    pub fn error(mut self) -> Error<'a> {
        self.msgs.sort();
        self.msgs.dedup();
        Error {
            help: self.help(),
            location: self.tokens[self.err_cursor].location,
            msgs: self.msgs,
        }
    }

    fn help(&self) -> Option<Help<'a>> {
        if self.tokens[self.err_cursor].lexeme == Lexeme::Name("else")
            && self.err_cursor != 0
            && self.tokens[self.err_cursor - 1].lexeme == Lexeme::Semicolon
        {
            Some(Help::RemoveSemicolon(
                self.tokens[self.err_cursor - 1].location,
            ))
        } else if self.msgs.contains(&Lexeme::Semicolon.show()) {
            let token = &self.tokens[self.err_cursor - 1];
            Some(Help::AddSemicolon(token.lexeme, token.location.after()))
        } else {
            None
        }
    }

    pub fn fail<T>(&mut self, s: &'a str) -> Result<T, Fail> {
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
    msgs: Vec<&'a str>,
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
    RemoveSemicolon(Location<'a>),
    AddSemicolon(Lexeme<'a>, Location<'a>),
}

impl Display for Help<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Help::RemoveSemicolon(location) => {
                write!(f, "try removing {Reset}`;`{Blue} in {location}")
            }
            Help::AddSemicolon(lexeme, location) => write!(
                f,
                "try adding {Reset}`;` {Blue}after {Reset}{} {Blue}in {}",
                lexeme.show(),
                location
            ),
        }
    }
}
