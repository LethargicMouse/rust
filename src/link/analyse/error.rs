use std::fmt::Display;

use crate::{
    Location,
    display::colors::{Blue, Red, Reset},
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

pub struct NotStruct<'a> {
    pub location: Location<'a>,
    pub name: &'a str,
}

impl Display for NotStruct<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! {Reset}`{}`{Red} is not struct or enum variant{Reset}",
            self.location, self.name
        )
    }
}

pub enum CheckError<'a> {
    NS(NotStruct<'a>),
    NCas(NoCast<'a>),
    Snt(ShouldKnowType<'a>),
    ND(NotDeclared<'a>),
    NF(NoField<'a>),
    NI(NoIndex<'a>),
    NCal(NoCall<'a>),
    WT(WrongType<'a>),
}

impl<'a> From<NotStruct<'a>> for CheckError<'a> {
    fn from(v: NotStruct<'a>) -> Self {
        Self::NS(v)
    }
}

impl<'a> From<NoCast<'a>> for CheckError<'a> {
    fn from(v: NoCast<'a>) -> Self {
        Self::NCas(v)
    }
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
        Self::NCal(v)
    }
}

impl<'a> From<NoIndex<'a>> for CheckError<'a> {
    fn from(v: NoIndex<'a>) -> Self {
        Self::NI(v)
    }
}

impl<'a> From<NotDeclared<'a>> for CheckError<'a> {
    fn from(v: NotDeclared<'a>) -> Self {
        Self::ND(v)
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
            CheckError::ND(not_declared) => write!(f, "{not_declared}"),
            CheckError::NF(no_field) => write!(f, "{no_field}"),
            CheckError::NI(no_deref) => write!(f, "{no_deref}"),
            CheckError::NCal(no_call) => write!(f, "{no_call}"),
            CheckError::WT(wrong_type) => write!(f, "{wrong_type}"),
            CheckError::Snt(should_know_type) => write!(f, "{should_know_type}"),
            CheckError::NCas(no_cast) => write!(f, "{no_cast}"),
            CheckError::NS(not_struct) => write!(f, "{not_struct}"),
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
    pub typ: Type<'a>,
}

impl Display for ShouldKnowType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! type should be known here{Reset}",
            self.location
        )?;
        if matches!(self.typ, Type::Unknown) {
            return Ok(());
        }
        write!(f, "\n{Blue}--@ inferred type is {Reset}{}", self.typ)
    }
}

pub struct NoCast<'a> {
    pub location: Location<'a>,
    pub from: Type<'a>,
    pub to: Type<'a>,
}

impl Display for NoCast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot cast {Reset}{}{Red} into {Reset}{}",
            self.location, self.from, self.to
        )
    }
}
