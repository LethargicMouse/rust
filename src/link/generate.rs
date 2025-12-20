use crate::{
    link::{Context, asg::*},
    qbe::ir::{self, IR, Stmt, Tmp, Type, Value},
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

    fn fun(&mut self, name: &str, fun: &Fun<'a>) -> ir::Fun {
        GenFun::new(self).run(name, fun)
    }
}

struct GenFun<'a, 'b> {
    sup: &'b mut Generate<'a>,
    stmts: Vec<Stmt>,
    tmp: Tmp,
    label: u16,
    context: Context<'a, Tmp>,
}

impl<'a, 'b> GenFun<'a, 'b> {
    fn new(sup: &'b mut Generate<'a>) -> Self {
        Self {
            stmts: Vec::new(),
            context: Context::new(),
            tmp: 0,
            sup,
            label: 0,
        }
    }

    fn run(mut self, name: &str, fun: &Fun<'a>) -> ir::Fun {
        let name = name.into();
        let params = fun.params.iter().map(|_| self.new_tmp()).collect();
        for (param, &tmp) in fun.params.iter().zip(&params) {
            self.context.insert(param, tmp);
        }
        for stmt in &fun.stmts {
            self.expr(stmt);
        }
        let tmp = self.expr(&fun.ret);
        self.stmts.push(Stmt::Ret(tmp));
        let stmts = self.stmts;
        ir::Fun {
            name,
            params,
            stmts,
        }
    }

    fn expr(&mut self, expr: &Expr) -> Tmp {
        match expr {
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(binary),
            Expr::Literal(literal) => self.literal(literal),
            Expr::Var(name) => self.var(name),
            Expr::If(if_expr) => self.if_expr(if_expr),
            Expr::Deref(expr) => self.deref(expr),
        }
    }

    fn deref(&mut self, expr: &Expr) -> Tmp {
        let expr = self.expr(expr);
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Load(tmp, expr));
        tmp
    }

    fn if_expr(&mut self, if_expr: &If) -> Tmp {
        let condition = self.expr(&if_expr.condition);
        let then_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        self.stmts
            .push(Stmt::Jnz(condition, then_label, else_label));
        self.stmts.push(Stmt::Label(then_label));
        let _ = self.expr(&if_expr.then_expr);
        self.stmts.push(Stmt::Jump(end_label));
        self.stmts.push(Stmt::Label(else_label));
        let _ = self.expr(&if_expr.else_expr);
        self.stmts.push(Stmt::Label(end_label));
        self.int(0)
    }

    fn new_label(&mut self) -> u16 {
        self.label += 1;
        self.label
    }

    fn var(&self, name: &str) -> Tmp {
        *self.context.get(name).unwrap()
    }

    fn call(&mut self, call: &Call) -> Tmp {
        let name = call.name.into();
        let args = call.args.iter().map(|e| self.expr(e)).collect();
        let tmp = self.new_tmp();
        self.stmts.push(ir::Call { tmp, name, args }.into());
        tmp
    }

    fn new_tmp(&mut self) -> Tmp {
        self.tmp += 1;
        self.tmp
    }

    fn binary(&mut self, binary: &Binary) -> Tmp {
        let left = self.expr(&binary.left);
        let right = self.expr(&binary.right);
        let op = self.bin_op(&binary.op);
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Bin(tmp, op, left, right));
        tmp
    }

    fn bin_op(&self, bin_op: &BinOp) -> ir::BinOp {
        match bin_op {
            BinOp::Add => ir::BinOp::Add,
            BinOp::Multiply => ir::BinOp::Multiply,
            BinOp::Equal => ir::BinOp::Equal,
        }
    }

    fn literal(&mut self, literal: &Literal) -> Tmp {
        match literal {
            Literal::Int(n) => self.int(*n),
            Literal::Str(s) => self.str(s),
        }
    }

    fn int(&mut self, n: i64) -> Tmp {
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Copy(tmp, Type::Long, n.into()));
        tmp
    }

    fn str(&mut self, s: &str) -> Tmp {
        let c = self.new_const(s);
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Copy(tmp, Type::Long, Value::Const(c)));
        tmp
    }

    fn new_const(&mut self, s: &str) -> u16 {
        self.sup.consts.push(s.into());
        self.sup.consts.len() as u16
    }
}
