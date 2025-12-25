mod error;
use std::collections::{HashMap, HashSet};

use error::Error;
mod after_structs;

use crate::{
    die,
    link::{
        Asg, Context,
        analyse::error::CheckError,
        ast::{Ast, Type},
    },
};

struct Typed<'a, T> {
    sup: T,
    typ: Type<'a>,
}

impl<'a, T> Typed<'a, T> {
    fn map_into<B: From<T>>(self) -> Typed<'a, B> {
        Typed {
            sup: self.sup.into(),
            typ: self.typ,
        }
    }
}

impl<'a, T> From<Typed<'a, T>> for (T, Type<'a>) {
    fn from(value: Typed<'a, T>) -> Self {
        (value.sup, value.typ)
    }
}

fn typed<'a, T>(sup: T, typ: Type<'a>) -> Typed<'a, T> {
    Typed { sup, typ }
}

pub fn analyse(ast: Ast) -> Asg {
    Analyse::new().run(ast)
}

pub struct Field<'a> {
    pub offset: usize,
    pub typ: Type<'a>,
}

pub struct Struct<'a> {
    pub fields: HashMap<&'a str, Field<'a>>,
}

struct State<'a> {
    errors: Vec<CheckError<'a>>,
    context: Context<'a, Type<'a>>,
    corrupt: HashSet<&'a str>,
}

impl<'a> State<'a> {
    fn new() -> Self {
        Self {
            context: Context::new(),
            errors: Vec::new(),
            corrupt: HashSet::new(),
        }
    }
}

struct Analyse<'a> {
    structs: HashMap<&'a str, Struct<'a>>,
    sup: State<'a>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        Self {
            sup: State::new(),
            structs: HashMap::new(),
        }
    }

    fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        let res = after_structs::Analyse::new(&mut self).run(ast);
        if !self.sup.errors.is_empty() {
            die(Error(self.sup.errors))
        }
        res
    }
}
