use std::fmt::Display;

use crate::{
    Location,
    display::colors::{Blue, Red, Reset},
    link::analyse::{Constraint, Type},
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
        if matches!(self.typ, Type::Unknown) {
            return write!(
                f,
                "{}\n{Red}--! type should be known here{Reset}",
                self.location
            );
        }
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
            "{}\n{Red}--! {Reset}`{}`{Red} is not a struct{Reset}",
            self.location, self.name
        )
    }
}

pub struct CheckError<'a> {
    kind: CheckErrorKind<'a>,
    pub help: Option<Help<'a>>,
}

impl Display for CheckError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)?;
        match &self.help {
            Some(help) => write!(f, "\n{help}"),
            None => Ok(()),
        }
    }
}

impl<'a, T: Into<CheckErrorKind<'a>>> From<T> for CheckError<'a> {
    fn from(val: T) -> Self {
        Self {
            kind: val.into(),
            help: None,
        }
    }
}

pub enum CheckErrorKind<'a> {
    CM(CantMatch<'a>),
    WC(WrongCount<'a>),
    R(Redeclared<'a>),
    Nil(NotInLoop<'a>),
    Nct(NotCompTime<'a>),
    NIm(NotImpl<'a>),
    NM(NoMethod<'a>),
    NA(NotAll<'a>),
    NS(NotStruct<'a>),
    NC(NoCast<'a>),
    Snt(ShouldKnowType<'a>),
    ND(NotDeclared<'a>),
    NF(NoField<'a>),
    NO(NoOp<'a>),
    WT(WrongType<'a>),
}

impl<'a> From<CantMatch<'a>> for CheckErrorKind<'a> {
    fn from(v: CantMatch<'a>) -> Self {
        Self::CM(v)
    }
}

impl<'a> From<WrongCount<'a>> for CheckErrorKind<'a> {
    fn from(v: WrongCount<'a>) -> Self {
        Self::WC(v)
    }
}

impl<'a> From<Redeclared<'a>> for CheckErrorKind<'a> {
    fn from(v: Redeclared<'a>) -> Self {
        Self::R(v)
    }
}

impl<'a> From<NotInLoop<'a>> for CheckErrorKind<'a> {
    fn from(v: NotInLoop<'a>) -> Self {
        Self::Nil(v)
    }
}

impl<'a> From<NotCompTime<'a>> for CheckErrorKind<'a> {
    fn from(v: NotCompTime<'a>) -> Self {
        Self::Nct(v)
    }
}

impl<'a> From<NotImpl<'a>> for CheckErrorKind<'a> {
    fn from(v: NotImpl<'a>) -> Self {
        Self::NIm(v)
    }
}

impl<'a> From<NoMethod<'a>> for CheckErrorKind<'a> {
    fn from(v: NoMethod<'a>) -> Self {
        Self::NM(v)
    }
}

impl<'a> From<NotAll<'a>> for CheckErrorKind<'a> {
    fn from(v: NotAll<'a>) -> Self {
        Self::NA(v)
    }
}

impl<'a> From<NotStruct<'a>> for CheckErrorKind<'a> {
    fn from(v: NotStruct<'a>) -> Self {
        Self::NS(v)
    }
}

impl<'a> From<NoCast<'a>> for CheckErrorKind<'a> {
    fn from(v: NoCast<'a>) -> Self {
        Self::NC(v)
    }
}

impl<'a> From<ShouldKnowType<'a>> for CheckErrorKind<'a> {
    fn from(v: ShouldKnowType<'a>) -> Self {
        Self::Snt(v)
    }
}

impl<'a> From<WrongType<'a>> for CheckErrorKind<'a> {
    fn from(v: WrongType<'a>) -> Self {
        Self::WT(v)
    }
}

impl<'a> From<NoOp<'a>> for CheckErrorKind<'a> {
    fn from(v: NoOp<'a>) -> Self {
        Self::NO(v)
    }
}

impl<'a> From<NotDeclared<'a>> for CheckErrorKind<'a> {
    fn from(v: NotDeclared<'a>) -> Self {
        Self::ND(v)
    }
}

impl<'a> From<NoField<'a>> for CheckErrorKind<'a> {
    fn from(v: NoField<'a>) -> Self {
        Self::NF(v)
    }
}

impl Display for CheckErrorKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{Red}! error checking ")?;
        match self {
            CheckErrorKind::ND(not_declared) => write!(f, "{not_declared}"),
            CheckErrorKind::NF(no_field) => write!(f, "{no_field}"),
            CheckErrorKind::NO(no_call) => write!(f, "{no_call}"),
            CheckErrorKind::WT(wrong_type) => write!(f, "{wrong_type}"),
            CheckErrorKind::Snt(should_know_type) => write!(f, "{should_know_type}"),
            CheckErrorKind::NC(no_cast) => write!(f, "{no_cast}"),
            CheckErrorKind::NS(not_struct) => write!(f, "{not_struct}"),
            CheckErrorKind::NA(not_all_fields) => write!(f, "{not_all_fields}"),
            CheckErrorKind::NM(no_method) => write!(f, "{no_method}"),
            CheckErrorKind::NIm(not_impl) => write!(f, "{not_impl}"),
            CheckErrorKind::Nct(not_comp_time) => write!(f, "{not_comp_time}"),
            CheckErrorKind::Nil(not_in_loop) => write!(f, "{not_in_loop}"),
            CheckErrorKind::R(redeclared) => write!(f, "{redeclared}"),
            CheckErrorKind::WC(wrong_count) => write!(f, "{wrong_count}"),
            CheckErrorKind::CM(cant_match) => write!(f, "{cant_match}"),
        }
    }
}

#[derive(Debug)]
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

pub struct NoOp<'a> {
    pub location: Location<'a>,
    pub typ: Type<'a>,
    pub op: &'a str,
}

impl Display for NoOp<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot {} a value of type {Reset}{}",
            self.location, self.op, self.typ
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

pub struct NotAll<'a> {
    pub location: Location<'a>,
    pub kind: &'a str,
    pub rest: Vec<&'a str>,
}

impl Display for NotAll<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! some {}s are not declared:{Blue}",
            self.location, self.kind
        )?;
        for field in &self.rest {
            write!(f, "\n    - {Reset}{field}{Blue}")?;
        }
        write!(f, "{Reset}")
    }
}

pub enum Help<'a> {
    Seb(SemicolonEndBlock<'a>),
}

impl Display for Help<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Help::Seb(semicolon_end_block) => write!(f, "{semicolon_end_block}"),
        }
    }
}

impl<'a> From<SemicolonEndBlock<'a>> for Help<'a> {
    fn from(v: SemicolonEndBlock<'a>) -> Self {
        Self::Seb(v)
    }
}

pub struct SemicolonEndBlock<'a> {
    pub location: Location<'a>,
}

impl Display for SemicolonEndBlock<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{Blue}--@ try removing {Reset}`;`{Blue} in {}",
            self.location
        )
    }
}

pub struct NoMethod<'a> {
    pub location: Location<'a>,
    pub name: &'a str,
    pub trait_name: &'a str,
}

impl Display for NoMethod<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! method {Reset}`{}` {Red}not found for trait {Reset}`{}`",
            self.location, self.name, self.trait_name
        )
    }
}

pub struct NotImpl<'a> {
    pub location: Location<'a>,
    pub typ: Type<'a>,
    pub constraint: Constraint<'a>,
}

impl Display for NotImpl<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! type {Reset}{} {Red}does not implement trait {Reset}`{}`\n{Blue}--@ ",
            self.location, self.typ, self.constraint
        )
    }
}

pub struct NotCompTime<'a> {
    pub location: Location<'a>,
}

impl Display for NotCompTime<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot evaluate this at compile time",
            self.location
        )
    }
}

pub struct NotInLoop<'a> {
    pub location: Location<'a>,
}

impl Display for NotInLoop<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot use {Reset}`break` {Red}outside of a loop",
            self.location
        )
    }
}

pub struct Redeclared<'a> {
    pub location: Location<'a>,
    pub kind: &'a str,
    pub name: &'a str,
    pub other: Location<'a>,
}

impl Display for Redeclared<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! {} {Reset}`{}` {Red}is already declared {Blue}in {}",
            self.location, self.kind, self.name, self.other
        )
    }
}

pub struct WrongCount<'a> {
    pub location: Location<'a>,
    pub expected: usize,
    pub found: usize,
}

impl Display for WrongCount<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! expected {Reset}{} {Red}arguments, got {Reset}{}",
            self.location, self.expected, self.found
        )
    }
}

pub struct CantMatch<'a> {
    pub location: Location<'a>,
    pub typ: Type<'a>,
}

impl Display for CantMatch<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{Red}--! cannot match a value of type {Reset}{}",
            self.location, self.typ
        )
    }
}
