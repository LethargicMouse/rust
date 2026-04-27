use std::collections::{HashMap, HashSet};

use crate::{
    good::{Context, asg::*},
    qbe::ir::{self, Const, IR, Signed, Stmt, Tmp, Unsigned, Value},
};

pub fn generate(asg: &Asg, debug: bool) -> IR {
    Generate::new(asg, debug).run()
}

pub struct TypeDeclInfo {
    offsets: Vec<Vec<u32>>,
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
    debug: bool,
}

impl<'a> Generate<'a> {
    fn new(asg: &'a Asg<'a>, debug: bool) -> Self {
        Self {
            types: Vec::new(),
            consts: Vec::new(),
            funs: Vec::new(),
            type_infos: HashMap::new(),
            booked: HashSet::new(),
            asg,
            context: Context::new(debug),
            debug,
        }
    }

    fn run(mut self) -> IR {
        for (name, (typ, const_)) in self.asg.consts.iter() {
            if self.debug {
                eprintln!("> const {name}")
            }
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
            Type::Ref(typ) => {
                *full_name += "$$ref";
                self.add_to_name(full_name, typ);
            }
            Type::Ptr(typ) => {
                *full_name += "$$ptr";
                self.add_to_name(full_name, typ);
            }
            Type::Cold(_) => unreachable!(),
            Type::Generic(_) => unreachable!(),
            Type::U64 => *full_name += "$$ul",
            Type::I32 => *full_name += "$$iw",
            Type::U8 => *full_name += "$$ub",
            Type::I64 => *full_name += "$$il",
            Type::F32 => *full_name += "$$fw",
            Type::F64 => *full_name += "$$fl",
            Type::Bool => *full_name += "$$bool",
            Type::Unit => *full_name += "$$unit",
            Type::FunPtr(items) => {
                *full_name += "$$fun";
                for typ in items {
                    self.add_to_name(full_name, typ);
                }
            }
            Type::Unknown => *full_name += "$$unit",
        }
    }

    fn data_type(&self, typ: &Type<'a>) -> ir::DataType {
        if self.debug {
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
            Type::Bool => Unsigned::Byte,
            Type::Cold(id) => self.unsigned(&self.asg.info.types[*id]),
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
            Type::Bool => ir::Type::Word,
            Type::Unit => ir::Type::Word,
            Type::Ref(_) => ir::Type::Long,
            Type::Ptr(_) => ir::Type::Long,
            Type::FunPtr(_) => ir::Type::Long,
            Type::Unknown => ir::Type::Word,
        }
    }

    fn eval_const(&mut self, expr: &'a Expr, typ: &Type<'a>) -> u16 {
        let mut contents = Vec::new();
        self.const_add_expr(expr, typ, &mut contents);
        self.new_const(Const::Struct(contents))
    }

    fn new_const(&mut self, s: Const) -> u16 {
        self.consts.push(s);
        self.consts.len() as u16
    }

    fn const_add_literal(
        &mut self,
        literal: &Literal<'_>,
        typ: &Type<'a>,
        contents: &mut Vec<(Unsigned, Value)>,
    ) {
        match literal {
            Literal::Int(i, _) => contents.push((self.unsigned(typ), int_val(*i, typ))),
            Literal::Str(s) => {
                let cs = self.new_const(Const::String((*s).into()));
                contents.push((ir::Type::Long.into(), Value::Const(cs)));
                contents.push((ir::Type::Long.into(), Value::Int(strlen(s))));
            }
            Literal::SizeOf(_) => todo!(),
        }
    }

    fn new_str(&mut self, s: &str) -> u16 {
        let cs = self.new_const(Const::String(s.into()));
        self.new_const(Const::Struct(vec![
            (ir::Type::Long.into(), Value::Const(cs)),
            (ir::Type::Long.into(), Value::Int(strlen(s))),
        ]))
    }

    fn const_add_expr(
        &mut self,
        expr: &'a Expr<'_>,
        typ: &Type<'a>,
        contents: &mut Vec<(Unsigned, Value)>,
    ) {
        match expr {
            Expr::Literal(literal) => self.const_add_literal(literal, typ, contents),
            Expr::Tuple(tuple) => self.const_add_tuple(tuple, contents),
            Expr::Var(name) => {
                if self.debug {
                    eprintln!("> const var {name}");
                }
                self.const_add_expr(&self.asg.consts[name].1, &self.asg.consts[name].0, contents)
            }
            _ => unreachable!("{expr:?}"),
        }
    }

    fn const_add_tuple(&mut self, tuple: &'a Tuple<'a>, contents: &mut Vec<(Unsigned, Value)>) {
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
    loop_ress: Vec<Tmp>,
    generics: &'c HashMap<&'a str, Type<'a>>,
    typ_generics: Vec<HashMap<&'a str, Type<'a>>>,
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
            typ_generics: Vec::new(),
            loop_ress: Vec::new(),
        }
    }

    fn run(mut self, name: &str, fun: &'a Fun<'a>) -> ir::Fun {
        if self.sup.debug {
            eprintln!("> generating fn {name}");
        }
        let params = fun
            .params
            .iter()
            .map(|(name, typ)| {
                if self.sup.debug {
                    eprintln!("> param {name}");
                }

                let typ = &self.heat_up(typ);
                self.size(typ); // for typ call
                (self.abi_type(typ), self.new_tmp())
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
        if self.sup.debug {
            eprintln!("> ret type");
        }
        let typ = self.heat_up(&fun.ret_type);
        let ret_type = self.abi_type(&typ);
        self.sup.context.pop_layer();
        if self.sup.debug {
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
        if self.sup.debug {
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

    fn expr(&mut self, expr: &Expr<'a>) -> Tmp {
        match expr {
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(binary),
            Expr::Literal(literal) => self.literal(literal),
            Expr::Var(name) => self.var(name),
            Expr::If(if_expr) => self.if_expr(if_expr),
            Expr::Deref(deref) => self.deref(&deref.expr, &deref.typ),
            Expr::Block(block) => self.block(block),
            Expr::Let(l) => self.let_expr(l.name, &l.expr, &l.typ),
            Expr::Field(field) => self.field(field),
            Expr::Assign(assign) => self.assign(assign),
            Expr::Tuple(exprs) => self.tuple(exprs),
            Expr::Loop(loop_expr) => self.loop_expr(loop_expr),
            Expr::Ref(ref_expr) => self.ref_expr(ref_expr),
            Expr::Return(ret) => self.ret(ret),
            Expr::Break(break_expr) => self.break_expr(break_expr),
            Expr::Cast(cast) => self.cast(cast),
            Expr::Negate(negate) => self.negate(negate),
            Expr::Expr(match_expr) => self.match_expr(match_expr),
            Expr::FunRef(fun_ref) => self.fun_ref(fun_ref),
        }
    }

    fn match_expr(&mut self, match_expr: &Match<'a>) -> Tmp {
        if self.sup.debug {
            eprintln!("> match expr")
        }
        let expr_tmp = self.expr(&match_expr.expr);
        let tmp = self.deref_tmp(expr_tmp, &Type::U8);
        let res = self.new_tmp();
        let end = self.new_label();
        let typ = self.heat_up(&match_expr.typ);
        for pm in &match_expr.pattern_matches {
            if self.sup.debug {
                eprintln!("> variant {}", pm.label);
            }
            let val = self.int(pm.label as i64, &Type::U8);
            let f = self.new_tmp();
            self.stmts.push(Stmt::Bin(
                f,
                self.sup.base(&Type::Bool),
                ir::BinOp::Equal(ir::Type::Word),
                val,
                tmp,
            ));
            let then = self.new_label();
            let next = self.new_label();
            self.stmts.push(Stmt::Jnz(f, then, next));
            self.stmts.push(Stmt::Label(then));
            self.sup.context.new_layer();
            if let Some((name, typp)) = &pm.mtyp {
                let typp = self.heat_up(typp);
                let expr_typ = self.heat_up(&match_expr.expr_typ);
                let offset = self.align(&expr_typ);
                let tmp_off = self.new_tmp();
                self.stmts.push(Stmt::Copy(
                    tmp_off,
                    self.sup.base(&expr_typ),
                    Value::Int(offset as i64),
                ));
                let offseted_tmp = self.new_tmp();
                self.stmts.push(Stmt::Bin(
                    offseted_tmp,
                    ir::Type::Long,
                    ir::BinOp::Add,
                    expr_tmp,
                    tmp_off,
                ));
                let var = self.deref_tmp(offseted_tmp, &typp);
                let var = self.store(var, &typp);
                self.sup.context.insert(name, (Value::Tmp(var), typp));
            }
            let expr = self.expr(&pm.expr);
            self.stmts
                .push(Stmt::Copy(res, self.sup.base(&typ), Value::Tmp(expr)));
            self.sup.context.pop_layer();
            self.stmts.push(Stmt::Jump(end));
            self.stmts.push(Stmt::Label(next));
        }
        self.stmts.push(Stmt::Label(end));
        res
    }

    fn ret(&mut self, ret: &Return<'a>) -> Tmp {
        let expr = self.expr(&ret.expr);
        self.stmts.push(Stmt::Ret(expr));
        let label = self.new_label();
        self.stmts.push(Stmt::Label(label));
        1
    }

    fn break_expr(&mut self, break_expr: &Break<'a>) -> Tmp {
        let expr = self.expr(&break_expr.expr);
        let res = *self.loop_ress.last().unwrap();
        let typ = self.heat_up(&break_expr.typ);
        let typ = self.sup.base(&typ);
        self.stmts.push(Stmt::Copy(res, typ, Value::Tmp(expr)));
        self.stmts.push(Stmt::Jump(*self.loop_ends.last().unwrap()));
        let label = self.new_label();
        self.stmts.push(Stmt::Label(label));
        expr
    }

    fn ref_expr(&mut self, ref_expr: &Ref<'a>) -> Tmp {
        self.expr_ref(&ref_expr.expr, &ref_expr.expr_typ)
    }

    fn negate(&mut self, negate: &Negate<'a>) -> Tmp {
        let expr = self.expr(&negate.expr);
        let typ = self.heat_up(&negate.expr_typ);
        let typ = self.sup.base(&typ);
        let res = self.new_tmp();
        self.stmts.push(Stmt::Neg(res, typ, expr));
        res
    }

    fn loop_expr(&mut self, loop_expr: &Loop<'a>) -> Tmp {
        let start = self.new_label();
        let end = self.new_label();
        let res = self.new_tmp();
        let typ = self.heat_up(&loop_expr.typ);
        let typ = self.sup.base(&typ);
        self.stmts.push(Stmt::Copy(res, typ, Value::Int(0)));
        self.loop_ends.push(end);
        self.loop_ress.push(res);
        self.stmts.push(Stmt::Label(start));
        self.block(&loop_expr.body);
        self.stmts.push(Stmt::Jump(start));
        self.stmts.push(Stmt::Label(end));
        res
    }

    fn tuple(&mut self, tuple: &Tuple<'a>) -> Tmp {
        if self.sup.debug {
            eprintln!("> tuple")
        }
        let tmp = self.new_tmp();
        let align = tuple
            .exprs
            .iter()
            .map(|(typ, _)| self.align(&self.heat_up(typ)))
            .max()
            .unwrap_or(1);
        let size = tuple
            .exprs
            .iter()
            .map(|(typ, _)| self.size(&self.heat_up(typ)).max(align))
            .sum();
        self.stmts.push(Stmt::Comment("tuple".into()));
        self.alloc(tmp, align, size);
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
            offset += self.size(&typ).max(align);
        }
        tmp
    }

    fn alloc(&mut self, tmp: u32, align: u32, size: u32) {
        self.stmts.push(if size == 0 {
            Stmt::Copy(tmp, ir::Type::Long, ir::Value::Int(0))
        } else {
            Stmt::Alloc(tmp, align, size)
        });
    }

    fn assign(&mut self, assign: &Assign<'a>) -> Tmp {
        let to = self.expr_ref(&assign.to, &assign.expr_type);
        let expr = self.expr(&assign.expr);
        let typ = self.heat_up(&assign.expr_type);
        self.put(to, expr, &typ);
        self.int(0, &Type::Unit)
    }

    fn size(&mut self, typ: &Type<'a>) -> u32 {
        if self.sup.debug {
            eprintln!("> size {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => {
                self.typ(name, generics);
                let full_name = self.sup.make_name(name, generics);
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
            Type::Bool => 1,
            Type::Unit => 0,
            Type::Ref(_) => 8,
            Type::Ptr(_) => 8,
            Type::FunPtr(_) => 8,
            Type::Unknown => 0,
        }
    }

    fn align(&mut self, typ: &Type<'a>) -> u32 {
        if self.sup.debug {
            eprintln!("> align {typ:?}");
        }
        match typ {
            Type::Name(name, generics) => {
                self.typ(name, generics);
                let full_name = self.sup.make_name(name, generics);
                self.sup.type_infos[&full_name].align
            }
            Type::Cold(id) => self.align(&self.sup.asg.info.types[*id]),
            Type::Generic(g) => self.align(&self.generics[g]),
            _ => self.size(typ),
        }
    }

    fn typ(&mut self, name: &str, generics: &[Type<'a>]) {
        let full_name = self.sup.make_name(name, generics);
        if self.sup.type_infos.contains_key(&full_name) {
            return;
        }
        if self.sup.debug {
            eprintln!("> making typ {full_name}");
        }
        let struct_ = &self.sup.asg.structs[name];
        self.typ_generics.push(
            struct_
                .generics
                .iter()
                .zip(generics)
                .map(|(n, t)| (*n, self.heat_up(t)))
                .collect(),
        );
        let mut variants = Vec::with_capacity(struct_.variants.len());
        let mut offsets = Vec::with_capacity(struct_.variants.len());
        let mut size = 0;
        let align = struct_
            .variants
            .iter()
            .flatten()
            .map(|f| self.align(&self.heat_up(f)))
            .max()
            .unwrap_or(1);
        for variant_fields in &struct_.variants {
            let mut variant_offsets = Vec::with_capacity(variant_fields.len());
            let mut fields = Vec::with_capacity(variant_fields.len());
            let mut offset = 0;
            for field in variant_fields {
                let field = &self.heat_up(field);
                let size = self.size(field);
                variant_offsets.push(offset);
                if size == 0 {
                    continue;
                }
                offset += size.max(align);
                fields.push(self.sup.data_type(field));
            }
            variants.push(fields);
            offsets.push(variant_offsets);
            size = size.max(offset);
        }
        self.typ_generics.pop();
        self.sup.types.push(ir::TypeDecl {
            name: full_name.clone(),
            variants,
        });
        self.sup.type_infos.insert(
            full_name.clone(),
            TypeDeclInfo {
                offsets,
                size,
                align,
            },
        );
    }

    fn put(&mut self, to: Tmp, from: Tmp, typ: &Type<'a>) {
        if matches!(typ, Type::Name(_, _)) {
            let size = self.size(typ);
            self.blit(to, from, size)
        } else {
            self.stmts
                .push(Stmt::Store(self.sup.unsigned(typ), from, to))
        }
    }

    fn blit(&mut self, to: Tmp, from: Tmp, size: u32) {
        if size != 0 {
            self.stmts.push(Stmt::Blit(to, from, size))
        }
    }

    fn expr_ref(&mut self, expr: &Expr<'a>, typ: &Type<'a>) -> Tmp {
        match expr {
            Expr::Var(s) => self.var_ref(s).0,
            Expr::Field(field) => self.field_ref(field),
            Expr::Deref(e) => self.expr(&e.expr),
            e => {
                let tmp = self.expr(e);
                let typ = self.heat_up(typ);
                self.store(tmp, &typ)
            }
        }
    }

    fn field_ref(&mut self, field: &Field<'a>) -> Tmp {
        if self.sup.debug {
            eprintln!("> {field:?}");
        }
        let from = self.expr_ref(&field.from, &field.from_type);
        let (name, generics) = match self.heat_up(&field.from_type) {
            Type::Name(name, generics) => (name, generics),
            _ => unreachable!(),
        };
        let off = self.new_tmp();
        self.typ(name, &generics);
        let full_name = self.sup.make_name(name, &generics);
        self.stmts.push(Stmt::Comment("offset".into()));
        let offset = self.int(
            self.sup.type_infos[&full_name].offsets[0][field.id] as i64,
            &Type::U64,
        );
        self.stmts
            .push(Stmt::Comment(format!("field {}", field.id)));
        self.stmts
            .push(Stmt::Bin(off, ir::Type::Long, ir::BinOp::Add, from, offset));
        off
    }

    fn field(&mut self, field: &Field<'a>) -> Tmp {
        let field_tmp = self.field_ref(field);
        let typ = self.heat_up(&field.typ);
        self.deref_tmp(field_tmp, &typ)
    }

    fn signed(&self, typ: &Type<'a>) -> Signed {
        match typ {
            Type::U8 => Signed::UnsignedByte,
            Type::Bool => Signed::UnsignedByte,
            _ => self.sup.base(typ).into(),
        }
    }

    fn let_expr(&mut self, name: &'a str, expr: &Expr<'a>, typ: &Type<'a>) -> Tmp {
        let tmp = self.expr(expr);
        let typ = self.heat_up(typ);
        let tmp = self.store(tmp, &typ);
        self.sup
            .context
            .insert(name, (Value::Tmp(tmp), typ.clone()));
        tmp
    }

    fn store(&mut self, tmp: Tmp, typ: &Type<'a>) -> Tmp {
        if matches!(typ, Type::Name(_, _)) {
            return tmp;
        }
        let res = self.new_tmp();
        let size = self.size(typ);
        self.alloc(res, size, size);
        self.stmts
            .push(Stmt::Store(self.sup.unsigned(typ), tmp, res));
        res
    }

    fn store_big(&mut self, tmp: Tmp, typ: &Type<'a>) -> Tmp {
        let res = self.new_tmp();
        let align = self.align(typ);
        let size = self.size(typ);
        self.alloc(res, align, size);
        self.put(res, tmp, typ);
        res
    }

    fn deref(&mut self, expr: &Expr<'a>, typ: &Type<'a>) -> Tmp {
        let expr = self.expr(expr);
        let typ = self.heat_up(typ);
        self.deref_tmp(expr, &typ)
    }

    fn deref_tmp(&mut self, expr: Tmp, typ: &Type<'a>) -> Tmp {
        if matches!(typ, Type::Name(_, _)) {
            return self.store_big(expr, typ);
        }
        let tmp = self.new_tmp();
        self.stmts.push(Stmt::Comment("deref".into()));
        self.stmts
            .push(Stmt::Load(tmp, self.sup.base(typ), self.signed(typ), expr));
        tmp
    }

    fn if_expr(&mut self, if_expr: &If<'a>) -> Tmp {
        let condition = self.expr(&if_expr.condition);
        let then_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        let res = self.new_tmp();
        let resized_condition = self.new_tmp();
        self.stmts.push(Stmt::Extub(resized_condition, condition));
        self.stmts
            .push(Stmt::Jnz(resized_condition, then_label, else_label));
        self.stmts.push(Stmt::Label(then_label));
        let then_expr = self.expr(&if_expr.then_expr);
        let typ = self.heat_up(&if_expr.typ);
        let typ = self.sup.base(&typ);
        self.stmts.push(Stmt::Copy(res, typ, Value::Tmp(then_expr)));
        self.stmts.push(Stmt::Jump(end_label));
        self.stmts.push(Stmt::Label(else_label));
        let else_expr = self.expr(&if_expr.else_expr);
        self.stmts.push(Stmt::Copy(res, typ, Value::Tmp(else_expr)));
        self.stmts.push(Stmt::Label(end_label));
        res
    }

    fn new_label(&mut self) -> u16 {
        self.label += 1;
        self.label
    }

    fn var_ref(&mut self, name: &str) -> (Tmp, Type<'a>) {
        if self.sup.debug {
            eprintln!("> var ref {name}");
        }
        self.stmts.push(Stmt::Comment(format!("ref var {name}")));
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
        self.deref_tmp(tmp, &typ)
    }

    fn call(&mut self, call: &Call<'a>) -> Tmp {
        if self.sup.debug {
            eprintln!("> {call:?}");
        }
        let name_tmp = self.expr(&call.expr);
        let tmp = self.new_tmp();
        if self.sup.debug {
            eprintln!("> ret type");
        }
        let ret_type = self.heat_up(&call.ret_type);
        self.size(&ret_type); // for type decls on extern funs
        let ret_type = self.abi_type(&ret_type);
        if self.sup.debug {
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
        self.stmts.push(
            ir::Call {
                tmp,
                ret_type,
                name: name_tmp,
                args,
            }
            .into(),
        );
        tmp
    }

    fn heat_up(&self, typ: &Type<'a>) -> Type<'a> {
        if self.sup.debug {
            eprintln!("> heat up {typ:?}");
        }
        match typ {
            Type::Cold(id) => self.heat_up(&self.sup.asg.info.types[*id]),
            Type::Generic(g) => self.heat_up(
                self.typ_generics
                    .last()
                    .unwrap_or(&HashMap::new())
                    .get(g)
                    .unwrap_or_else(|| &self.generics[g]),
            ),
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

    fn binary(&mut self, binary: &Binary<'a>) -> Tmp {
        let left = self.expr(&binary.left);
        let right = self.expr(&binary.right);
        let typ = self.heat_up(&binary.args_typ);
        let op = self.bin_op(&binary.op, &typ);
        let tmp = self.new_tmp();
        let typ = self.heat_up(&binary.typ);
        self.stmts
            .push(Stmt::Bin(tmp, self.sup.base(&typ), op, left, right));
        tmp
    }

    fn bin_op(&self, bin_op: &BinOp, typ: &Type<'a>) -> ir::BinOp {
        if self.sup.debug {
            eprintln!("> binop {typ:?}");
        }
        match bin_op {
            BinOp::Add => ir::BinOp::Add,
            BinOp::Multiply => ir::BinOp::Multiply,
            BinOp::Equal => ir::BinOp::Equal(self.sup.base(typ)),
            BinOp::Less => ir::BinOp::Less(self.sup.base(typ)),
            BinOp::NotEqual => ir::BinOp::Inequal(self.sup.base(typ)),
            BinOp::Modulo => ir::BinOp::Urem,
            BinOp::Divide => ir::BinOp::Div,
            BinOp::And => ir::BinOp::And,
            BinOp::Subtract => ir::BinOp::Sub,
            BinOp::Or => ir::BinOp::Or,
            BinOp::More => ir::BinOp::More(self.sup.base(typ)),
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
        let val = int_val(n, &typ);
        self.stmts.push(Stmt::Copy(tmp, self.sup.base(&typ), val));
        tmp
    }

    fn str(&mut self, s: &str) -> Tmp {
        let c = self.sup.new_str(s);
        let tmp = self.new_tmp();
        self.stmts
            .push(Stmt::Copy(tmp, ir::Type::Long, Value::Const(c)));
        tmp
    }

    fn block(&mut self, block: &Block<'a>) -> u32 {
        for stmt in &block.stmts {
            self.expr(stmt);
        }
        self.expr(&block.ret)
    }

    fn cast(&mut self, cast: &Cast<'a>) -> u32 {
        let expr = self.expr(&cast.expr);
        let from = self.heat_up(&cast.from);
        let to = self.heat_up(&cast.to);
        match (from, to) {
            (_, Type::Unit) => self.int(0, &Type::Unit),
            (Type::F32, to) if to.is_i() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Stosi(self.sup.base(&to), res, expr));
                res
            }
            (Type::F32, to) if to.is_u() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Stoui(self.sup.base(&to), res, expr));
                res
            }
            (Type::F64, to) if to.is_i() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Dtosi(self.sup.base(&to), res, expr));
                res
            }
            (Type::I32, to) if to.is_f() => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Swtof(self.sup.base(&to), res, expr));
                res
            }
            (Type::U8, Type::I64) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Extub(res, expr));
                res
            }
            (Type::I32, Type::U64) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Extsw(res, expr));
                res
            }
            (Type::F32, Type::F64) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Exts(res, expr));
                res
            }
            (Type::F64, Type::F32) => {
                let res = self.new_tmp();
                self.stmts.push(Stmt::Truncd(res, expr));
                res
            }
            _ => expr,
        }
    }

    fn unify(&mut self, a: &Type<'a>, b: &Type<'a>) -> bool {
        if self.sup.debug {
            eprintln!("> unify {a:?} {b:?}")
        }
        match (a, b) {
            (Type::Cold(id), t) => self.unify(&self.sup.asg.info.types[*id], t),
            (Type::Generic(g), t)
                if self.unify(
                    &self.typ_generics.last().unwrap_or(&HashMap::new())[g].clone(),
                    t,
                ) =>
            {
                *self
                    .typ_generics
                    .last_mut()
                    .unwrap_or(&mut HashMap::new())
                    .get_mut(g)
                    .unwrap() = t.clone();
                true
            }
            (Type::Unknown, _) => true,
            (_, Type::Unknown) => true,
            (Type::Name(a, ga), Type::Name(b, gb)) if a == b => {
                ga.iter().zip(gb).all(|(a, b)| self.unify(a, b))
            }
            (a, b) if a == b => true,
            _ => false,
        }
    }

    fn fun_ref(&mut self, fun_ref: &FunRef<'a>) -> Tmp {
        if self.sup.debug {
            eprintln!("> fun ref {}", fun_ref.name);
            eprintln!("> generics");
        }
        let (mut g_names, mut generics): (Vec<_>, Vec<_>) = fun_ref
            .generics
            .iter()
            .map(|(name, typ)| (name, self.heat_up(typ)))
            .unzip();
        let mut name: String = self.sup.make_name(fun_ref.name, &generics);
        if let Some(fun) = self.sup.asg.funs.get(fun_ref.name).or_else(|| {
            let (gc, impls) = self.sup.asg.trait_funs.get(fun_ref.name)?;
            for impl_ in impls {
                // FIXME this does not work and will never work because all the codebase is cursed!!!
                self.typ_generics
                    .push(impl_.generics.iter().map(|n| (*n, Type::Unknown)).collect());
                if self.sup.debug {
                    eprintln!("> candidate self");
                }
                if self.unify(&impl_.typ, &generics[*gc])
                    && generics[0..*gc]
                        .iter()
                        .zip(&impl_.trait_generics)
                        .all(|(f, e)| self.unify(e, f))
                {
                    for g in self.typ_generics.pop().unwrap() {
                        g_names.push(g.0);
                        generics.push(g.1);
                    }
                    return Some(&impl_.fun);
                }
                self.typ_generics.pop();
            }
            None
        }) {
            if !self.sup.booked.contains(&name) {
                self.sup.booked.insert(name.clone());
                let generics = &g_names.into_iter().zip(generics).collect();
                let fun = GenFun::new(self.sup, generics).run(&name, fun);
                self.sup.funs.push(fun);
            }
        } else {
            name = fun_ref.name.into();
        }
        if self.sup.debug {
            eprintln!("> got name {name}");
        }
        let name = Value::Fun(name);
        let name_tmp = self.new_tmp();
        self.stmts
            .push(ir::Stmt::Copy(name_tmp, ir::Type::Long, name));
        name_tmp
    }
}

fn int_val(n: i64, typ: &Type<'_>) -> Value {
    match *typ {
        Type::F32 => Value::Float(n as f32),
        Type::F64 => Value::Double(n as f64),
        _ => Value::Int(n),
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
