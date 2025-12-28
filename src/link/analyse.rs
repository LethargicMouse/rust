mod error;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use error::Error;
mod after_structs;

use crate::{
    die,
    display::Sep,
    link::{
        Asg, Context,
        analyse::error::CheckError,
        ast::{Ast, Prime},
    },
};

#[derive(Clone)]
struct FunType<'a> {
    params: Vec<Type<'a>>,
    ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({}) {}", Sep(", ", &self.params), self.ret_type)
    }
}

#[derive(Clone)]
enum Type<'a> {
    Ptr(Box<Type<'a>>),
    Name(&'a str),
    Fun(Box<FunType<'a>>),
    Prime(Prime),
    Error,
}

impl<'a> From<Prime> for Type<'a> {
    fn from(v: Prime) -> Self {
        Self::Prime(v)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Ptr(typ) => write!(f, "*{typ}"),
            Type::Name(n) => write!(f, "{n}"),
            Type::Error => write!(f, "<error>"),
            Type::Fun(fun_type) => write!(f, "{fun_type}"),
            Type::Prime(prime) => write!(f, "{prime}"),
        }
    }
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

impl<'a> Type<'a> {
    fn name(&self) -> Option<&'a str> {
        match self {
            Type::Name(n) => Some(n),
            _ => None,
        }
    }
}

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

struct Field<'a> {
    pub offset: usize,
    pub typ: Type<'a>,
}

struct Struct<'a> {
    fields: HashMap<&'a str, Field<'a>>,
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
