use crate::link::{
    Asg, asg,
    ast::{Ast, BinOp, Binary, Call, Expr, Fun, Get, If, Literal},
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
            Expr::Call(call) => self.call(call).into(),
            Expr::Binary(binary) => self.binary(binary).into(),
            Expr::Literal(literal) => self.literal(literal).into(),
            Expr::Var(name) => asg::Expr::Var(name),
            Expr::If(if_expr) => self.if_expr(if_expr).into(),
            Expr::Get(get) => self.get(get),
        }
    }

    fn get(&self, get: Get<'a>) -> asg::Expr<'a> {
        let from = self.expr(*get.from);
        let index = self.expr(*get.index);
        asg::Expr::Deref(Box::new(
            asg::Binary {
                left: Box::new(from),
                right: Box::new(
                    asg::Binary {
                        left: Box::new(index),
                        right: Box::new(asg::Literal::Int(8).into()),
                        op: asg::BinOp::Multiply,
                    }
                    .into(),
                ),
                op: asg::BinOp::Add,
            }
            .into(),
        ))
    }

    fn if_expr(&self, if_expr: If<'a>) -> asg::If<'a> {
        let condition = Box::new(self.expr(*if_expr.condition));
        let then_expr = Box::new(self.expr(*if_expr.then_expr));
        asg::If {
            condition,
            then_expr,
        }
    }

    fn call(&self, call: Call<'a>) -> asg::Call<'a> {
        let args = call.args.into_iter().map(|e| self.expr(e)).collect();
        let name = call.name;
        asg::Call { name, args }
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
            BinOp::Equal => asg::BinOp::Equal,
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
