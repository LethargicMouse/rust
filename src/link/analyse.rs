mod error;
use std::collections::HashMap;

use error::Error;
mod expr;

use crate::{
    die,
    link::{
        Asg, Context,
        analyse::error::CheckError,
        asg,
        ast::{Ast, Fun, Type},
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

struct Analyse<'a> {
    structs: HashMap<&'a str, Struct<'a>>,
    errors: Vec<CheckError<'a>>,
    context: Context<'a, Type<'a>>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        Self {
            context: Context::new(),
            errors: Vec::new(),
            structs: HashMap::new(),
        }
    }

    fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        for extrn in ast.externs {
            self.context.insert(extrn.name, extrn.typ);
        }
        for fun in &ast.funs {
            self.context
                .insert(fun.header.name, fun.header.typ.clone().into());
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|f| (f.header.name, self.fun(f)))
            .collect();
        if !self.errors.is_empty() {
            die(Error(self.errors))
        }
        Asg { funs }
    }

    pub fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        self.context.new_layer();
        for (param, typ) in fun.header.params.iter().zip(fun.header.typ.params) {
            self.context.insert(param, typ);
        }
        let params = fun.header.params;
        let body = expr::Analyse::new(self).expr(fun.body).sup;
        asg::Fun { params, body }
    }
}
