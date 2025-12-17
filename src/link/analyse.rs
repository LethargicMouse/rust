use crate::link::{
    Asg, asg,
    ast::{Ast, BinOp, Binary, Call, Expr, Fun, Literal},
};

pub fn analyse(ast: Ast) -> Asg {
    Analyse::new().run(ast)
}

struct Analyse {}

impl<'a> Analyse {
    fn new() -> Self {
        Self {}
    }

    fn run(self, ast: Ast) -> Asg {
        let funs = ast
            .funs
            .into_iter()
            .map(|(n, f)| (n, self.fun(f)))
            .collect();
        Asg { funs }
    }

    fn fun(&self, fun: Fun<'a>) -> asg::Fun<'a> {
        let params = fun.params;
        let stmts = fun.stmts.into_iter().map(|e| self.expr(e)).collect();
        let ret = self.expr(fun.ret);
        asg::Fun { params, stmts, ret }
    }

    fn expr(&self, expr: Expr<'a>) -> asg::Expr<'a> {
        match expr {
            Expr::Call(call) => asg::Expr::Call(self.call(call)),
            Expr::Binary(binary) => asg::Expr::Binary(self.binary(binary)),
            Expr::Literal(literal) => asg::Expr::Literal(self.literal(literal)),
            Expr::Var(name) => asg::Expr::Var(name),
        }
    }

    fn call(&self, call: Call<'a>) -> asg::Call<'a> {
        let arg = Box::new(self.expr(*call.arg));
        let name = call.name;
        asg::Call { arg, name }
    }

    fn binary(&self, binary: Binary<'a>) -> asg::Binary<'a> {
        let left = Box::new(self.expr(*binary.left));
        let right = Box::new(self.expr(*binary.right));
        let op = self.bin_op(binary.op);
        asg::Binary { left, op, right }
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
        }
    }

    fn literal(&self, literal: Literal<'a>) -> asg::Literal<'a> {
        match literal {
            Literal::Unit => asg::Literal::Int(0),
            Literal::Int(i) => asg::Literal::Int(i),
            Literal::RawStr(s) => asg::Literal::Str(s),
        }
    }
}
