mod gen_stmts;

use crate::{
    link::{
        analyse::gen_stmts::GenStmts,
        ast::{Ast, Expr},
    },
    qbe::ir::{IR, Stmt},
};

pub fn analyse(ast: Ast) -> IR {
    Analyse::new().run(ast)
}

struct Analyse {
    consts: Vec<String>,
}

impl Analyse {
    fn new() -> Self {
        let consts = Vec::new();
        Self { consts }
    }

    fn run(mut self, ast: Ast) -> IR {
        let stmts = self.gen_stmts(ast.expr);
        let consts = self.consts;
        IR { stmts, consts }
    }

    fn gen_stmts(&mut self, expr: Expr) -> Vec<Stmt> {
        GenStmts::new(&mut self.consts).run(expr)
    }
}
