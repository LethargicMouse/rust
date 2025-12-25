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
        for extrn in ast.externs {
            self.sup.context.insert(extrn.name, extrn.typ);
        }
        for fun in &ast.funs {
            self.sup
                .context
                .insert(fun.header.name, fun.header.typ.clone().into());
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|fun| {
                (
                    fun.header.name,
                    after_structs::Analyse::new(&mut self).fun(fun),
                )
            })
            .collect();
        if !self.sup.errors.is_empty() {
            die(Error(self.sup.errors))
        }
        Asg { funs }
    }
}
