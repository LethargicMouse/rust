use crate::{
    link::asg::*,
    qbe::ir::{self, IR, Stmt, Tmp, Value},
};

pub fn generate(asg: &Asg) -> IR {
    Generate::new(asg).run()
}

struct Generate<'a> {
    consts: Vec<String>,
    asg: &'a Asg<'a>,
}

impl<'a> Generate<'a> {
    fn new(asg: &'a Asg<'a>) -> Self {
        let consts = Vec::new();
        Self { consts, asg }
    }

    fn run(mut self) -> IR {
        let funs = self.asg.funs.iter().map(|(n, f)| self.fun(n, f)).collect();
        let consts = self.consts;
        IR { consts, funs }
    }

    fn fun(&mut self, name: &str, fun: &Fun) -> ir::Fun {
        let stmts = GenStmts::new(self).run(&fun.stmts, &fun.ret);
        let name = name.into();
        ir::Fun { name, stmts }
    }
}

struct GenStmts<'a, 'b> {
    sup: &'b mut Generate<'a>,
    stmts: Vec<Stmt>,
    tmp: Tmp,
}

impl<'a, 'b> GenStmts<'a, 'b> {
    fn new(sup: &'b mut Generate<'a>) -> Self {
        Self {
            stmts: Vec::new(),
            tmp: 0,
            sup,
        }
    }

    fn run(mut self, stmts: &Vec<Expr>, ret: &Expr) -> Vec<Stmt> {
        for stmt in stmts {
            self.expr(stmt);
        }
        let tmp = self.expr(ret);
        self.stmts.push(Stmt::Ret(tmp));
        self.stmts
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
        self.sup.consts.push(s.into());
        self.sup.consts.len() as u16
    }
}
