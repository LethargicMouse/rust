use crate::{
    link::ast::{BinOp, Binary, Call, Expr, Literal},
    qbe::ir::{self, Stmt, Tmp},
};

pub fn gen_stmts(expr: Expr) -> Vec<Stmt> {
    let mut gen_stmts = GenStmts::new();
    let tmp = gen_stmts.expr(expr);
    gen_stmts.result.push(Stmt::Ret(tmp));
    gen_stmts.result
}

struct GenStmts {
    result: Vec<Stmt>,
    next_tmp: Tmp,
}

impl GenStmts {
    fn new() -> Self {
        Self {
            result: Vec::new(),
            next_tmp: 1,
        }
    }

    fn expr(&mut self, expr: Expr) -> Tmp {
        match expr {
            Expr::Literal(l) => self.literal(l),
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(binary),
        }
    }

    fn literal(&mut self, literal: Literal) -> Tmp {
        match literal {
            Literal::Unit => self.unit(),
            Literal::Int(n) => self.int(n),
            Literal::RawStr(s) => self.raw_str(s),
        }
    }

    fn call(&mut self, call: Call) -> Tmp {
        let arg = self.expr(*call.arg);
        let tmp = self.next_tmp();
        self.result.push(Stmt::Call(tmp, call.name.into(), arg));
        tmp
    }

    fn unit(&mut self) -> Tmp {
        self.int(0)
    }

    fn int(&mut self, n: i32) -> Tmp {
        let tmp = self.next_tmp();
        self.result.push(Stmt::Copy(tmp, n));
        tmp
    }

    fn next_tmp(&mut self) -> Tmp {
        let res = self.next_tmp;
        self.next_tmp += 1;
        res
    }

    fn binary(&mut self, binary: Binary<'_>) -> u32 {
        let left = self.expr(*binary.left);
        let right = self.expr(*binary.right);
        let op = match binary.op {
            BinOp::Plus => ir::BinOp::Add,
        };
        let tmp = self.next_tmp();
        self.result.push(Stmt::Bin(tmp, op, left, right));
        tmp
    }

    fn raw_str(&mut self, _raw_str: &'_ str) -> u32 {
        todo!()
    }
}
