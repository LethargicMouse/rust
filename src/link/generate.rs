use crate::{
    link::asg::*,
    qbe::ir::{self, IR, Stmt, Tmp, Value},
};

pub fn generate(asg: &Asg) -> IR {
    Generate::new(asg).run()
}

struct Generate<'a> {
    consts: Vec<String>,
    stmts: Vec<Stmt>,
    asg: &'a Asg<'a>,
    tmp: Tmp,
}

impl<'a> Generate<'a> {
    fn new(asg: &'a Asg<'a>) -> Self {
        Self {
            consts: Vec::new(),
            stmts: Vec::new(),
            tmp: 0,
            asg,
        }
    }

    fn run(mut self) -> IR {
        for stmt in &self.asg.stmts {
            self.expr(stmt);
        }
        let tmp = self.expr(&self.asg.ret);
        self.stmts.push(Stmt::Ret(tmp));
        let consts = self.consts;
        let stmts = self.stmts;
        IR { consts, stmts }
    }

    fn expr(&mut self, expr: &Expr) -> Tmp {
        match expr {
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(binary),
            Expr::Literal(literal) => self.literal(literal),
        }
    }

    fn call(&mut self, call: &Call) -> Tmp {
        let arg = self.expr(&call.arg);
        let tmp = self.next_tmp();
        self.stmts.push(Stmt::Call(tmp, call.name.into(), arg));
        tmp
    }

    fn next_tmp(&mut self) -> Tmp {
        self.tmp += 1;
        self.tmp
    }

    fn binary(&mut self, binary: &Binary) -> Tmp {
        let left = self.expr(&binary.left);
        let right = self.expr(&binary.right);
        let op = self.bin_op(&binary.op);
        let tmp = self.next_tmp();
        self.stmts.push(Stmt::Bin(tmp, op, left, right));
        tmp
    }

    fn bin_op(&self, bin_op: &BinOp) -> ir::BinOp {
        match bin_op {
            BinOp::Add => ir::BinOp::Add,
        }
    }

    fn literal(&mut self, literal: &Literal) -> Tmp {
        match literal {
            Literal::Int(n) => self.int(*n),
            Literal::Str(s) => self.str(s),
        }
    }

    fn int(&mut self, n: i32) -> Tmp {
        let tmp = self.next_tmp();
        self.stmts.push(Stmt::Copy(tmp, n.into()));
        tmp
    }

    fn str(&mut self, s: &str) -> Tmp {
        let c = self.new_const(s);
        let tmp = self.next_tmp();
        self.stmts.push(Stmt::Copy(tmp, Value::Const(c)));
        tmp
    }

    fn new_const(&mut self, s: &str) -> u16 {
        self.consts.push(s.into());
        self.consts.len() as u16
    }
}
