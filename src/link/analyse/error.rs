use std::fmt::Display;

use crate::{
    Location,
    display::colors::{Red, Reset},
    link::analyse::Type,
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

pub struct NoField<'a> {
    pub location: Location<'a>,
    pub name: &'a str,
    pub typ: Type<'a>,
}

impl Display for NoField<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! field {Reset}`{}`{Red} not found for type {Reset}{}",
            self.location, self.name, self.typ
        )
    }
}

pub enum CheckError<'a> {
    Snt(ShouldKnowType<'a>),
    NDec(NotDeclared<'a>),
    NF(NoField<'a>),
    MDer(NoIndex<'a>),
    NC(NoCall<'a>),
    WT(WrongType<'a>),
}

impl<'a> From<ShouldKnowType<'a>> for CheckError<'a> {
    fn from(v: ShouldKnowType<'a>) -> Self {
        Self::Snt(v)
    }
}

impl<'a> From<WrongType<'a>> for CheckError<'a> {
    fn from(v: WrongType<'a>) -> Self {
        Self::WT(v)
    }
}

impl<'a> From<NoCall<'a>> for CheckError<'a> {
    fn from(v: NoCall<'a>) -> Self {
        Self::NC(v)
    }
}

impl<'a> From<NoIndex<'a>> for CheckError<'a> {
    fn from(v: NoIndex<'a>) -> Self {
        Self::MDer(v)
    }
}

impl<'a> From<NotDeclared<'a>> for CheckError<'a> {
    fn from(v: NotDeclared<'a>) -> Self {
        Self::NDec(v)
    }
}

impl<'a> From<NoField<'a>> for CheckError<'a> {
    fn from(v: NoField<'a>) -> Self {
        Self::NF(v)
    }
}

impl Display for CheckError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{Red}! error checking ")?;
        match self {
            CheckError::NDec(not_declared) => write!(f, "{not_declared}"),
            CheckError::NF(no_field) => write!(f, "{no_field}"),
            CheckError::MDer(no_deref) => write!(f, "{no_deref}"),
            CheckError::NC(no_call) => write!(f, "{no_call}"),
            CheckError::WT(wrong_type) => write!(f, "{wrong_type}"),
            CheckError::Snt(should_know_type) => write!(f, "{should_know_type}"),
        }
    }
}

pub struct Fail;

pub struct Error<'a>(pub Vec<CheckError<'a>>);

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.0 {
            writeln!(f, "{error}\n")?;
        }
        write!(
            f,
            "\n{Red}! check failed with {Reset}{}{Red} error{}{Reset}",
            self.0.len(),
            if self.0.len() == 1 { "" } else { "s" }
        )
    }
}

pub struct NoIndex<'a> {
    pub location: Location<'a>,
    pub typ: Type<'a>,
}

impl Display for NoIndex<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot index a value of type {Reset}{}",
            self.location, self.typ
        )
    }
}

pub struct NoCall<'a> {
    pub location: Location<'a>,
    pub typ: Type<'a>,
}

impl Display for NoCall<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot call a value of type {Reset}{}",
            self.location, self.typ
        )
    }
}

pub struct WrongType<'a> {
    pub location: Location<'a>,
    pub expected: Type<'a>,
    pub found: Type<'a>,
}

impl Display for WrongType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! mismatched types:\n----! expected {Reset}{}{Red}\n----!    found {Reset}{}",
            self.location, self.expected, self.found
        )
    }
}

pub struct ShouldKnowType<'a> {
    pub location: Location<'a>,
}

impl Display for ShouldKnowType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! type should be known here{Reset}",
            self.location
        )
    }
}
