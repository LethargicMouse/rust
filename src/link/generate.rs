use crate::{
    link::{Context, asg::*},
    qbe::ir::{self, Const, IR, Signed, Stmt, Tmp, Unsigned, Value},
};

pub fn generate(asg: &Asg) -> IR {
    Generate::new(asg).run()
}

struct Generate<'a> {
    consts: Vec<Const>,
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
    context: Context<'a, (Tmp, Type<'a>)>,
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
        let params = fun.params.iter().map(|_| self.new_tmp()).collect();
        for ((param, typ), &tmp) in fun.params.iter().zip(&params) {
            let tmp = self.store(tmp, self.align(typ), self.size(typ));
            self.context.insert(param, (tmp, typ.clone()));
        }
        let tmp = self.expr(&fun.body);
        self.stmts.push(Stmt::Ret(tmp));
        ir::Fun {
            ret_type: self.abi_type(&fun.ret_type),
            name: name.into(),
            params,
            stmts: self.stmts,
        }
    }

    fn abi_type(&self, typ: &Type<'a>) -> ir::AbiType {
        match typ {
            Type::Name { name, .. } => ir::AbiType::Name(name.to_string()),
            Type::U64 => ir::Type::Long.into(),
            Type::I32 => ir::Type::Word.into(),
            Type::U8 => Signed::UnsignedByte.into(),
            Type::Cold(id) => self.abi_type(&self.sup.asg.info.types[*id]),
        }
    }

    fn expr(&mut self, expr: &Expr<'a>) -> Tmp {
        match expr {
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(binary),
            Expr::Literal(literal) => self.literal(literal),
            Expr::Var(name) => self.var(name),
            Expr::If(if_expr) => self.if_expr(if_expr),
            Expr::Deref(expr) => self.deref(expr),
            Expr::Block(block) => self.block(block),
            Expr::Let(let_expr) => self.let_expr(let_expr),
            Expr::Field(field) => self.field(field),
            Expr::Assign(assign) => self.assign(assign),
            Expr::Tuple(exprs) => self.tuple(exprs),
            Expr::Loop(loop_expr) => self.loop_expr(loop_expr),
            Expr::Ref(ref_expr) => self.ref_expr(ref_expr),
            Expr::Return(ret) => self.ret(ret),
        }
    }

    fn ret(&mut self, ret: &Return<'a>) -> Tmp {
        let expr = self.expr(&ret.expr);
        self.stmts.push(Stmt::Ret(expr));
        let label = self.new_label();
        self.stmts.push(Stmt::Label(label));
        self.int(0)
    }

    fn ref_expr(&mut self, ref_expr: &Ref<'a>) -> Tmp {
        self.expr_ref(&ref_expr.expr)
    }

    fn loop_expr(&mut self, loop_expr: &Loop<'a>) -> Tmp {
        let start = self.new_label();
        self.stmts.push(Stmt::Label(start));
        let body = self.block(&loop_expr.body);
        self.stmts.push(Stmt::Jump(start));
        let end = self.new_label();
        self.stmts.push(Stmt::Label(end));
        body
    }

    fn tuple(&mut self, tuple: &Tuple<'a>) -> Tmp {
        let tmp = self.new_tmp();
        let size = tuple.exprs.iter().map(|e| e.size).sum();
        self.stmts.push(Stmt::Alloc(tmp, tuple.align, size));
        let offset = 0;
        for sized in &tuple.exprs {
            let expr = self.expr(&sized.expr);
            let offset_tmp = self.new_tmp();
            self.stmts.push(Stmt::Copy(
                offset_tmp,
                ir::Type::Long,
                (offset as i64).into(),
            ));
            let with_offset = self.new_tmp();
            self.stmts
                .push(Stmt::Bin(with_offset, ir::BinOp::Add, tmp, offset));
            self.copy(with_offset, expr, sized.size);
        }
        tmp
    }

    fn assign(&mut self, assign: &Assign<'a>) -> Tmp {
        let to = self.expr_ref(&assign.to);
        let expr = self.expr(&assign.expr);
        self.copy(to, expr, self.size(&assign.expr_type));
        self.int(0)
    }

    fn size(&self, typ: &Type<'a>) -> u32 {
        match typ {
            Type::Name { size, .. } => *size,
            Type::Cold(id) => self.size(&self.sup.asg.info.types[*id]),
            Type::U64 => 8,
            Type::I32 => 4,
            Type::U8 => 1,
        }
    }

    fn align(&self, typ: &Type<'a>) -> u32 {
        match typ {
            Type::Name { align, .. } => *align,
            Type::Cold(id) => self.align(&self.sup.asg.info.types[*id]),
            _ => self.size(typ),
        }
    }

    fn copy(&mut self, to: Tmp, from: Tmp, size: u32) {
        let stmt = match size {
            1 => Stmt::Store(Unsigned::Byte, from, to),
            2 => Stmt::Store(Unsigned::Half, from, to),
            4 => Stmt::Store(ir::Type::Word.into(), from, to),
            8 => Stmt::Store(ir::Type::Long.into(), from, to),
            _ => Stmt::Blit(to, from, size),
        };
        self.stmts.push(stmt);
    }

    fn expr_ref(&mut self, expr: &Expr<'a>) -> Tmp {
        match expr {
            Expr::Var(s) => self.var_ref(s).0,
            Expr::Field(field) => self.field_ref(field),
            e => unreachable!("{e:?}"),
        }
    }

    fn field_ref(&mut self, field: &Field<'a>) -> Tmp {
        let from = self.expr_ref(&field.from);
        let off = self.new_tmp();
        let offset = self.int(field.offset as i64);
        self.stmts
            .push(Stmt::Bin(off, ir::BinOp::Add, from, offset));
        off
    }

    fn field(&mut self, field: &Field<'a>) -> Tmp {
        let typ = self.signed(&field.typ);
        let field = self.field_ref(field);
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Load(tmp, typ, field));
        tmp
    }

    fn signed(&self, typ: &Type<'a>) -> Signed {
        match typ {
            Type::Name { .. } => ir::Type::Long.into(),
            Type::U64 => ir::Type::Long.into(),
            Type::I32 => ir::Type::Word.into(),
            Type::U8 => Signed::UnsignedByte,
            Type::Cold(id) => self.signed(&self.sup.asg.info.types[*id]),
        }
    }

    fn let_expr(&mut self, let_expr: &Let<'a>) -> Tmp {
        let tmp = self.expr(&let_expr.expr);
        let size = self.size(&let_expr.typ);
        let tmp = self.store(tmp, self.align(&let_expr.typ), size);
        self.context
            .insert(let_expr.name, (tmp, let_expr.typ.clone()));
        self.int(0)
    }

    fn store(&mut self, tmp: Tmp, align: u32, size: u32) -> Tmp {
        let res = self.new_tmp();
        self.stmts.push(Stmt::Alloc(res, align, size));
        self.copy(res, tmp, size);
        res
    }

    fn deref(&mut self, deref: &Deref<'a>) -> Tmp {
        let expr = self.expr(&deref.expr);
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Load(tmp, self.signed(&deref.typ), expr));
        tmp
    }

    fn if_expr(&mut self, if_expr: &If<'a>) -> Tmp {
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

    fn var_ref(&self, name: &str) -> (Tmp, Type<'a>) {
        self.context.get(name).unwrap().clone()
    }

    fn var(&mut self, name: &str) -> Tmp {
        let (tmp, typ) = self.var_ref(name);
        let res = self.new_tmp();
        self.stmts.push(Stmt::Load(res, self.signed(&typ), tmp));
        res
    }

    fn call(&mut self, call: &Call<'a>) -> Tmp {
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

    fn binary(&mut self, binary: &Binary<'a>) -> Tmp {
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
            BinOp::Less => ir::BinOp::Less,
            BinOp::NotEqual => ir::BinOp::Inequal,
        }
    }

    fn literal(&mut self, literal: &Literal) -> Tmp {
        match literal {
            Literal::Int(n) => self.int(*n),
            Literal::RawStr(s) => self.raw_str(s),
            Literal::Str(s) => self.str(s),
        }
    }

    fn int(&mut self, n: i64) -> Tmp {
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Copy(tmp, ir::Type::Long, n.into()));
        tmp
    }

    fn raw_str(&mut self, s: &str) -> Tmp {
        let c = self.new_const(Const::String(s.into()));
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Copy(tmp, ir::Type::Long, Value::Const(c)));
        tmp
    }

    fn str(&mut self, s: &str) -> Tmp {
        let cs = self.new_const(Const::String(s.into()));
        let c = self.new_const(Const::Struct(vec![Value::Const(cs), Value::Int(strlen(s))]));
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Copy(tmp, ir::Type::Long, Value::Const(c)));
        tmp
    }

    fn new_const(&mut self, s: Const) -> u16 {
        self.sup.consts.push(s);
        self.sup.consts.len() as u16
    }

    fn block(&mut self, block: &Block<'a>) -> u32 {
        for stmt in &block.stmts {
            self.expr(stmt);
        }
        self.expr(&block.ret)
    }
}

fn strlen(mut s: &str) -> i64 {
    let mut res = 0;
    while !s.is_empty() {
        if s.starts_with("\\") {
            s = &s[1..];
        }
        s = &s[1..];
        res += 1;
    }
    res
}
