mod error;
mod fun;

use crate::link::{
    Asg, Context, asg,
    ast::{Ast, Fun},
};

pub fn analyse(ast: Ast) -> Asg {
    Analyse::new().run(ast)
}

struct Analyse<'a> {
    context: Context<'a, ()>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        let context = Context::new();
        Self { context }
    }

    fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        for extrn in ast.externs {
            self.context.insert(extrn, ());
        }
        for fun in &ast.funs {
            self.context.insert(fun.name, ());
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|f| (f.name, self.fun(f)))
            .collect();
        Asg { funs }
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        fun::Analyse::new(&mut self.context).run(fun)
    }
}
