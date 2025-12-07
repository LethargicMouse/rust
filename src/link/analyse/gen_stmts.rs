use crate::{
    link::ast::{BinOp, Binary, Call, Expr, Literal},
    qbe::ir::{self, Stmt, Tmp, Value},
};

pub struct GenStmts<'a> {
    consts: &'a mut Vec<String>,
    result: Vec<Stmt>,
    next_tmp: Tmp,
}

impl<'a> GenStmts<'a> {
    pub fn new(consts: &'a mut Vec<String>) -> Self {
        Self {
            consts,
            result: Vec::new(),
            next_tmp: 1,
        }
    }

    pub fn run(mut self, expr: Expr) -> Vec<Stmt> {
        let tmp = self.expr(expr);
        self.result.push(Stmt::Ret(tmp));
        self.result
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
        self.result.push(Stmt::Copy(tmp, Value::Int(n)));
        tmp
    }

    fn next_tmp(&mut self) -> Tmp {
        let res = self.next_tmp;
        self.next_tmp += 1;
        res
    }

    fn binary(&mut self, binary: Binary) -> Tmp {
        let left = self.expr(*binary.left);
        let right = self.expr(*binary.right);
        let op = match binary.op {
            BinOp::Plus => ir::BinOp::Add,
        };
        let tmp = self.next_tmp();
        self.result.push(Stmt::Bin(tmp, op, left, right));
        tmp
    }

    fn raw_str(&mut self, raw_str: &str) -> Tmp {
        self.consts.push(raw_str.into());
        let tmp = self.next_tmp();
        self.result
            .push(Stmt::Copy(tmp, Value::Const(self.consts.len() as u16)));
        tmp
    }
}
