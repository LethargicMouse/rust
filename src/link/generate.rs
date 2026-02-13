use std::collections::{HashMap, HashSet};

use crate::{
    link::{Context, analyse::DEBUG, asg::*},
    qbe::ir::{self, Const, DataType, IR, Signed, Stmt, Tmp, Unsigned, Value},
};

pub fn generate(asg: &Asg) -> IR {
    Generate::new(asg).run()
}

pub struct TypeDeclInfo {
    offsets: Vec<u32>,
    size: u32,
    align: u32,
}

struct Generate<'a> {
    types: Vec<ir::TypeDecl>,
    consts: Vec<Const>,
    funs: Vec<ir::Fun>,
    type_infos: HashMap<String, TypeDeclInfo>,
    asg: &'a Asg<'a>,
    booked: HashSet<String>,
    context: Context<'a, (Value, Type<'a>)>,
}

impl<'a> Generate<'a> {
    fn new(asg: &'a Asg<'a>) -> Self {
        Self {
            types: Vec::new(),
            consts: Vec::new(),
            funs: Vec::new(),
            type_infos: HashMap::new(),
            booked: HashSet::new(),
            asg,
            context: Context::new(),
        }
    }

    fn run(mut self) -> IR {
        for (name, typ, const_) in self.asg.consts.iter() {
            let id = self.eval_const(const_, typ);
            self.context.insert(name, (Value::Const(id), typ.clone()))
        }
        let fun = &self.asg.funs["main"];
        let main = GenFun::new(&mut self, &HashMap::new()).run("main", fun);
        self.funs.push(main);
        IR {
            types: self.types,
            consts: self.consts,
            funs: self.funs,
        }
    }

    fn make_name(&self, name: &str, generics: &[Type<'a>]) -> String {
        let mut full_name = name.to_string();
        for typ in generics {
            self.add_to_name(&mut full_name, typ);
        }
        full_name
    }

    fn add_to_name(&self, full_name: &mut String, typ: &Type<'a>) {
        match typ {
            Type::Name(name, generics) => {
                *full_name += "$";
                *full_name += name;
                for typ in generics {
                    self.add_to_name(full_name, typ);
                }
            }
            Type::Cold(_) => unreachable!(),
            Type::Generic(_) => unreachable!(),
            Type::U64 => *full_name += "$$ul",
            Type::I32 => *full_name += "$$iw",
            Type::U8 => *full_name += "$$ub",
            Type::I64 => *full_name += "$$il",
            Type::F32 => *full_name += "$$fw",
            Type::F64 => *full_name += "$$fl",
        }
    }

    fn data_type(&self, typ: &Type<'a>) -> ir::DataType {
        if DEBUG {
            eprintln!("> data_type {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => ir::DataType::Name(self.make_name(name, generics)),
            _ => self.unsigned(typ).into(),
        }
    }

    fn unsigned(&self, typ: &Type<'a>) -> Unsigned {
        match typ {
            Type::U8 => Unsigned::Byte,
            _ => self.base(typ).into(),
        }
    }

    fn base(&self, typ: &Type<'a>) -> ir::Type {
        match typ {
            Type::Name(_, _) => ir::Type::Long,
            Type::Cold(_) => unreachable!(),
            Type::Generic(_) => unreachable!(),
            Type::U8 => ir::Type::Word,
            Type::U64 => ir::Type::Long,
            Type::I32 => ir::Type::Word,
            Type::I64 => ir::Type::Long,
            Type::F32 => ir::Type::Float,
            Type::F64 => ir::Type::Double,
        }
    }

    fn eval_const(&mut self, expr: &Expr, typ: &Type<'a>) -> u16 {
        let mut contents = Vec::new();
        self.const_add_expr(expr, typ, &mut contents);
        self.new_const(Const::Struct(contents))
    }

    fn new_const(&mut self, s: Const) -> u16 {
        self.consts.push(s);
        self.consts.len() as u16
    }

    fn const_add_literal(
        &self,
        literal: &Literal<'_>,
        typ: &Type<'a>,
        contents: &mut Vec<(DataType, Value)>,
    ) {
        match literal {
            Literal::Int(i, _) => contents.push((self.data_type(typ), Value::Int(*i))),
            Literal::Str(_) => todo!(),
            Literal::SizeOf(_) => todo!(),
        }
    }

    fn const_add_expr(
        &self,
        expr: &Expr<'_>,
        typ: &Type<'a>,
        contents: &mut Vec<(DataType, Value)>,
    ) {
        match expr {
            Expr::Literal(literal) => self.const_add_literal(literal, typ, contents),
            Expr::Tuple(tuple) => self.const_add_tuple(tuple, contents),
            _ => unreachable!("{expr:?}"),
        }
    }

    fn const_add_tuple(&self, tuple: &'a Tuple<'a>, contents: &mut Vec<(DataType, Value)>) {
        for (typ, expr) in &tuple.exprs {
            self.const_add_expr(expr, typ, contents);
        }
    }
}

struct GenFun<'a, 'b, 'c> {
    sup: &'b mut Generate<'a>,
    stmts: Vec<Stmt>,
    tmp: Tmp,
    label: u16,
    loop_ends: Vec<u16>,
    generics: &'c HashMap<&'a str, Type<'a>>,
}

impl<'a, 'b, 'c> GenFun<'a, 'b, 'c> {
    fn new(sup: &'b mut Generate<'a>, generics: &'c HashMap<&'a str, Type<'a>>) -> Self {
        Self {
            stmts: Vec::new(),
            loop_ends: Vec::new(),
            tmp: 0,
            label: 0,
            generics,
            sup,
        }
    }

    fn run(mut self, name: &str, fun: &'a Fun<'a>) -> ir::Fun {
        if DEBUG {
            eprintln!("> generating fn {name}");
        }
        let params = fun
            .params
            .iter()
            .map(|(name, typ)| {
                if DEBUG {
                    eprintln!("> param {name}");
                }
                (self.abi_type(&self.heat_up(typ)), self.new_tmp())
            })
            .collect();
        self.sup.context.new_layer();
        for ((param, typ), &(_, tmp)) in fun.params.iter().zip(&params) {
            let typ = self.heat_up(typ);
            let tmp = self.store(tmp, &typ);
            self.sup
                .context
                .insert(param, (Value::Tmp(tmp), typ.clone()));
        }
        let tmp = self.expr(&fun.body);
        self.stmts.push(Stmt::Ret(tmp));
        if DEBUG {
            eprintln!("> ret type");
        }
        let typ = self.heat_up(&fun.ret_type);
        let ret_type = self.abi_type(&typ);
        self.sup.context.pop_layer();
        if DEBUG {
            eprintln!("> out fn {name}");
        }
        ir::Fun {
            ret_type,
            name: name.into(),
            params,
            stmts: self.stmts,
        }
    }

    fn abi_type(&self, typ: &Type<'a>) -> ir::AbiType {
        if DEBUG {
            eprintln!("> abi_type {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => {
                let full_name = self.sup.make_name(name, generics);
                ir::AbiType::Name(full_name)
            }
            _ => self.signed(typ).into(),
        }
    }

    fn expr(&mut self, expr: &'a Expr<'a>) -> Tmp {
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
            Expr::Break(break_expr) => self.break_expr(break_expr),
            Expr::Cast(cast) => self.cast(cast),
        }
    }

    fn ret(&mut self, ret: &'a Return<'a>) -> Tmp {
        let expr = self.expr(&ret.expr);
        self.stmts.push(Stmt::Ret(expr));
        let label = self.new_label();
        self.stmts.push(Stmt::Label(label));
        1
    }

    fn break_expr(&mut self, break_expr: &'a Break<'a>) -> Tmp {
        let expr = self.expr(&break_expr.expr);
        self.stmts.push(Stmt::Jump(*self.loop_ends.last().unwrap()));
        let label = self.new_label();
        self.stmts.push(Stmt::Label(label));
        expr
    }

    fn ref_expr(&mut self, ref_expr: &'a Ref<'a>) -> Tmp {
        self.expr_ref(&ref_expr.expr, &ref_expr.expr_typ)
    }

    fn loop_expr(&mut self, loop_expr: &'a Loop<'a>) -> Tmp {
        let start = self.new_label();
        let end = self.new_label();
        self.loop_ends.push(end);
        self.stmts.push(Stmt::Label(start));
        let body = self.block(&loop_expr.body);
        self.stmts.push(Stmt::Jump(start));
        self.stmts.push(Stmt::Label(end));
        body
    }

    fn tuple(&mut self, tuple: &'a Tuple<'a>) -> Tmp {
        let tmp = self.new_tmp();
        let (align, size) = tuple
            .exprs
            .iter()
            .map(|(typ, _)| (self.align(typ), self.size(typ)))
            .fold((1, 0), |(fa, fs), (a, s)| (fa.max(a), fs + s));
        self.stmts.push(Stmt::Alloc(tmp, align, size));
        let mut offset = 0;
        for (typ, expr) in &tuple.exprs {
            let expr = self.expr(expr);
            let offset_tmp = self.new_tmp();
            self.stmts.push(Stmt::Copy(
                offset_tmp,
                ir::Type::Long,
                (offset as i64).into(),
            ));
            let with_offset = self.new_tmp();
            self.stmts.push(Stmt::Bin(
                with_offset,
                ir::Type::Long,
                ir::BinOp::Add,
                tmp,
                offset_tmp,
            ));
            let typ = self.heat_up(typ);
            self.put(with_offset, expr, &typ);
            offset += self.size(&typ);
        }
        tmp
    }

    fn assign(&mut self, assign: &'a Assign<'a>) -> Tmp {
        let to = self.expr_ref(&assign.to, &assign.expr_type);
        let expr = self.expr(&assign.expr);
        let typ = self.heat_up(&assign.expr_type);
        self.put(to, expr, &typ);
        expr
    }

    fn size(&mut self, typ: &Type<'a>) -> u32 {
        if DEBUG {
            eprintln!("> size {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => {
                let full_name = self.sup.make_name(name, generics);
                self.typ(&full_name, name);
                self.sup.type_infos[&full_name].size
            }
            Type::Cold(id) => self.size(&self.sup.asg.info.types[*id]),
            Type::Generic(g) => self.size(&self.generics[g]),
            Type::U64 => 8,
            Type::I32 => 4,
            Type::U8 => 1,
            Type::I64 => 8,
            Type::F32 => 4,
            Type::F64 => 8,
        }
    }

    fn align(&mut self, typ: &Type<'a>) -> u32 {
        if DEBUG {
            eprintln!("> align {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => {
                let full_name = self.sup.make_name(name, generics);
                self.typ(&full_name, name);
                self.sup.type_infos[&full_name].align
            }
            Type::Cold(id) => self.align(&self.sup.asg.info.types[*id]),
            _ => self.size(typ),
        }
    }

    fn typ(&mut self, full_name: &String, name: &str) {
        if DEBUG {
            eprintln!("> making typ {full_name}");
        }
        if self.sup.type_infos.contains_key(full_name) {
            return;
        }
        let mut offset = 0;
        let mut align = 1;
        let mut offsets = Vec::new();
        let struct_ = &self.sup.asg.structs[name];
        let mut fields = Vec::new();
        offsets.reserve(struct_.fields.len());
        fields.reserve(struct_.fields.len());
        for field in &struct_.fields {
            offsets.push(offset);
            offset += self.size(field);
            align = align.max(self.align(field));
            fields.push(self.sup.data_type(field));
        }
        self.sup.types.push(ir::TypeDecl {
            name: full_name.clone(),
            fields,
        });
        self.sup.type_infos.insert(
            full_name.clone(),
            TypeDeclInfo {
                offsets,
                size: offset,
                align,
            },
        );
    }

    fn put(&mut self, to: Tmp, from: Tmp, typ: &Type<'a>) {
        let stmt = if matches!(typ, Type::Name(_, _)) {
            Stmt::Blit(to, from, self.size(typ))
        } else {
            Stmt::Store(self.sup.unsigned(typ), from, to)
        };
        self.stmts.push(stmt);
    }

    fn expr_ref(&mut self, expr: &'a Expr<'a>, typ: &Type<'a>) -> Tmp {
        match expr {
            Expr::Var(s) => self.var_ref(s).0,
            Expr::Field(field) => self.field_ref(field),
            e => {
                let tmp = self.expr(e);
                let typ = self.heat_up(typ);
                self.store(tmp, &typ)
            }
        }
    }

    fn field_ref(&mut self, field: &'a Field<'a>) -> Tmp {
        if DEBUG {
            eprintln!("> {field:?}");
        }
        let from = self.expr_ref(&field.from, &field.from_type);
        let (name, generics) = match self.heat_up(&field.from_type) {
            Type::Name(name, generics) => (name, generics),
            _ => unreachable!(),
        };
        let off = self.new_tmp();
        let full_name = self.sup.make_name(name, &generics);
        self.typ(&full_name, name);
        let offset = self.int(
            self.sup.type_infos[&full_name].offsets[field.id] as i64,
            &Type::U64,
        );
        self.stmts
            .push(Stmt::Bin(off, ir::Type::Long, ir::BinOp::Add, from, offset));
        off
    }

    fn field(&mut self, field: &'a Field<'a>) -> Tmp {
        let field_tmp = self.field_ref(field);
        if matches!(field.typ, Type::Name(_, _)) {
            return field_tmp;
        }
        let tmp = self.new_tmp();
        let typ = self.heat_up(&field.typ);
        let base = self.sup.base(&typ);
        let typ = self.signed(&typ);
        self.stmts.push(Stmt::Load(tmp, base, typ, field_tmp));
        tmp
    }

    fn signed(&self, typ: &Type<'a>) -> Signed {
        match typ {
            Type::U8 => Signed::UnsignedByte,
            _ => self.sup.base(typ).into(),
        }
    }

    fn let_expr(&mut self, let_expr: &'a Let<'a>) -> Tmp {
        let tmp = self.expr(&let_expr.expr);
        let typ = self.heat_up(&let_expr.typ);
        let tmp = self.store(tmp, &typ);
        self.sup
            .context
            .insert(let_expr.name, (Value::Tmp(tmp), let_expr.typ.clone()));
        tmp
    }

    fn store(&mut self, tmp: Tmp, typ: &Type<'a>) -> Tmp {
        if matches!(typ, Type::Name(_, _)) {
            return tmp;
        }
        let res = self.new_tmp();
        let size = self.size(typ);
        self.stmts.push(Stmt::Alloc(res, size, size));
        self.stmts
            .push(Stmt::Store(self.sup.unsigned(typ), tmp, res));
        res
    }

    fn deref(&mut self, deref: &'a Deref<'a>) -> Tmp {
        let expr = self.expr(&deref.expr);
        if matches!(deref.typ, Type::Name(_, _)) {
            return expr;
        }
        let tmp = self.new_tmp();
        let typ = self.heat_up(&deref.typ);
        self.stmts.push(Stmt::Load(
            tmp,
            self.sup.base(&typ),
            self.signed(&typ),
            expr,
        ));
        tmp
    }

    fn if_expr(&mut self, if_expr: &'a If<'a>) -> Tmp {
        let condition = self.expr(&if_expr.condition);
        let then_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        self.stmts
            .push(Stmt::Jnz(condition, then_label, else_label));
        self.stmts.push(Stmt::Label(then_label));
        let then = self.expr(&if_expr.then_expr);
        self.stmts.push(Stmt::Jump(end_label));
        self.stmts.push(Stmt::Label(else_label));
        let _ = self.expr(&if_expr.else_expr);
        self.stmts.push(Stmt::Label(end_label));
        then
    }

    fn new_label(&mut self) -> u16 {
        self.label += 1;
        self.label
    }

    fn var_ref(&mut self, name: &str) -> (Tmp, Type<'a>) {
        let (val, typ) = self.sup.context.get(name).unwrap().clone();
        let tmp = match val {
            Value::Tmp(tmp) => tmp,
            _ => {
                let tmp = self.new_tmp();
                self.stmts.push(Stmt::Copy(tmp, ir::Type::Long, val));
                tmp
            }
        };
        (tmp, typ)
    }

    fn var(&mut self, name: &str) -> Tmp {
        let (tmp, typ) = self.var_ref(name);
        let typ = self.heat_up(&typ);
        if matches!(typ, Type::Name(_, _)) {
            return tmp;
        }
        let res = self.new_tmp();
        self.stmts
            .push(Stmt::Load(res, self.sup.base(&typ), self.signed(&typ), tmp));
        res
    }

    fn call(&mut self, call: &'a Call<'a>) -> Tmp {
        if DEBUG {
            eprintln!("> {call:?}");
        }
        let (g_names, generics): (Vec<_>, Vec<_>) = call
            .generics
            .iter()
            .map(|(name, typ)| (name, self.heat_up(typ)))
            .unzip();
        let mut first_g_name = String::new();
        self.sup.add_to_name(
            &mut first_g_name,
            &generics.first().cloned().unwrap_or_default().clone(),
        );
        let name: String = self.sup.make_name(call.name, &generics);
        if let Some(fun) = self.sup.asg.funs.get(call.name).or_else(|| {
            for (typ, fun) in self
                .sup
                .asg
                .trait_funs
                .get(call.name)
                .into_iter()
                .flat_map(|v| v.iter())
            {
                let mut buf = String::new();
                self.sup.add_to_name(&mut buf, typ);
                if first_g_name == buf {
                    return Some(fun);
                }
            }
            None
        }) && !self.sup.booked.contains(&name)
        {
            self.sup.booked.insert(name.clone());
            let generics = &g_names.into_iter().zip(generics).collect();
            let fun = GenFun::new(self.sup, generics).run(&name, fun);
            self.sup.funs.push(fun);
        }
        if DEBUG {
            eprintln!("> ret type");
        }
        let ret_type = self.heat_up(&call.ret_type);
        self.size(&ret_type); // for type decls on extern funs
        let ret_type = self.abi_type(&ret_type);
        if DEBUG {
            eprintln!("> args");
        }
        let args = call
            .args
            .iter()
            .map(|(typ, expr)| {
                let typ = self.heat_up(typ);
                self.size(&typ); // for type decls on extern funs
                (self.abi_type(&typ), self.expr(expr))
            })
            .collect();
        let tmp = self.new_tmp();
        self.stmts.push(
            ir::Call {
                tmp,
                ret_type,
                name,
                args,
            }
            .into(),
        );
        tmp
    }

    fn heat_up(&self, typ: &Type<'a>) -> Type<'a> {
        match typ {
            Type::Cold(id) => self.heat_up(&self.sup.asg.info.types[*id]),
            Type::Generic(g) => self.heat_up(&self.generics[g]),
            Type::Name(name, generics) => {
                let generics = generics.iter().map(|typ| self.heat_up(typ)).collect();
                Type::Name(name, generics)
            }
            typ => typ.clone(),
        }
    }

    fn new_tmp(&mut self) -> Tmp {
        self.tmp += 1;
        self.tmp
    }

    fn binary(&mut self, binary: &'a Binary<'a>) -> Tmp {
        let left = self.expr(&binary.left);
        let right = self.expr(&binary.right);
        let op = self.bin_op(&binary.op);
        let tmp = self.new_tmp();
        let typ = self.heat_up(&binary.typ);
        self.stmts
            .push(Stmt::Bin(tmp, self.sup.base(&typ), op, left, right));
        tmp
    }

    fn bin_op(&self, bin_op: &BinOp) -> ir::BinOp {
        match bin_op {
            BinOp::Add => ir::BinOp::Add,
            BinOp::Multiply => ir::BinOp::Multiply,
            BinOp::Equal => ir::BinOp::Equal,
            BinOp::Less => ir::BinOp::Less,
            BinOp::NotEqual => ir::BinOp::Inequal,
            BinOp::Modulo => ir::BinOp::Urem,
            BinOp::Divide => ir::BinOp::Udiv,
            BinOp::And => ir::BinOp::And,
            BinOp::Subtract => ir::BinOp::Sub,
        }
    }

    fn literal(&mut self, literal: &Literal<'a>) -> Tmp {
        match literal {
            Literal::Int(n, typ) => self.int(*n, typ),
            Literal::Str(s) => self.str(s),
            Literal::SizeOf(typ) => {
                let size = self.size(typ);
                self.int(size as i64, &Type::U64)
            }
        }
    }

    fn int(&mut self, n: i64, typ: &Type<'a>) -> Tmp {
        let tmp = self.new_tmp();
        let typ = self.heat_up(typ);
        let val = match typ {
            Type::F32 => Value::Float(n as f32),
            Type::F64 => Value::Double(n as f64),
            _ => Value::Int(n),
        };
        self.stmts.push(Stmt::Copy(tmp, self.sup.base(&typ), val));
        tmp
    }

    fn str(&mut self, s: &str) -> Tmp {
        let cs = self.sup.new_const(Const::String(s.into()));
        let c = self.sup.new_const(Const::Struct(vec![
            (ir::Type::Long.into(), Value::Const(cs)),
            (ir::Type::Long.into(), Value::Int(strlen(s))),
        ]));
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Copy(tmp, ir::Type::Long, Value::Const(c)));
        tmp
    }

    fn block(&mut self, block: &'a Block<'a>) -> u32 {
        for stmt in &block.stmts {
            self.expr(stmt);
        }
        self.expr(&block.ret)
    }

    fn cast(&mut self, cast: &'a Cast<'a>) -> u32 {
        let expr = self.expr(&cast.expr);
        let from = self.heat_up(&cast.from);
        let to = self.heat_up(&cast.to);
        match (from, to) {
            (Type::F32, to) if to.is_i() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Stosi(self.sup.base(&to), res, expr));
                res
            }
            (Type::F64, to) if to.is_i() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Dtosi(self.sup.base(&to), res, expr));
                res
            }
            (Type::U8, Type::I64) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Extub(res, expr));
                res
            }
            (Type::F32, Type::F64) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Exts(res, expr));
                res
            }
            _ => expr,
        }
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
