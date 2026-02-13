mod error;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    mem::take,
};

use error::Error;

use crate::{
    Location, die,
    display::Sep,
    link::{
        Asg, Context,
        analyse::error::{
            CheckError, Fail, NoCall, NoCast, NoField, NoIndex, NoMethod, NotAll, NotCompTime,
            NotDeclared, NotImpl, NotInLoop, NotStruct, SemicolonEndBlock, WrongType,
        },
        asg::{self, Tuple},
        ast::{self, *},
    },
};

pub const DEBUG: bool = false;

#[derive(Clone, PartialEq, Debug)]
struct FunType<'a> {
    generics: Vec<Generic<'a>>,
    params: Vec<Type<'a>>,
    ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({})", Sep(", ", &self.params))?;
        if matches!(self.ret_type, Type::Prime(Prime::Unit)) {
            Ok(())
        } else {
            write!(f, " {}", self.ret_type)
        }
    }
}

#[derive(Clone, PartialEq, Default, Debug)]
enum Type<'a> {
    Ptr(Box<Type<'a>>),
    Ref(Box<Type<'a>>),
    Name(&'a str, Vec<Type<'a>>),
    Fun(Box<FunType<'a>>),
    Prime(Prime),
    #[default]
    Error,
    Number,
    Var(usize),
    Unreachable,
    Generic(&'a str),
    Unknown(Option<&'a str>),
}

impl<'a> From<Prime> for Type<'a> {
    fn from(v: Prime) -> Self {
        Self::Prime(v)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Ptr(typ) => write!(f, "*{typ}"),
            Type::Name(n, gs) => {
                write!(f, "{n}")?;
                if gs.is_empty() {
                    return Ok(());
                }
                write!(f, "<{}", gs[0])?;
                for g in &gs[1..] {
                    write!(f, ", {g}")?;
                }
                write!(f, ">")
            }
            Type::Error => write!(f, "_"),
            Type::Fun(fun_type) => write!(f, "{fun_type}"),
            Type::Prime(prime) => write!(f, "{prime}"),
            Type::Number => write!(f, "<number>"),
            Type::Var(id) => write!(f, "<#{id}>"),
            Type::Unreachable => write!(f, "<unreachable>"),
            Type::Generic(n) => write!(f, "{n}"),
            Type::Unknown(None) => write!(f, "_"),
            Type::Unknown(Some(c)) => write!(f, "impl {c}"),
            Type::Ref(typ) => write!(f, "&{typ}"),
        }
    }
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

impl<'a> Type<'a> {
    fn is_number(&self) -> bool {
        match self {
            Type::Ptr(_) => true,
            Type::Prime(prime) => prime.is_number(),
            Type::Error => true,
            Type::Number => true,
            Type::Unreachable => true,
            _ => false,
        }
    }
}

struct Typed<'a, T> {
    sup: T,
    typ: Type<'a>,
}

impl<'a, T> Typed<'a, T> {
    fn map_into<B: From<T>>(self) -> Typed<'a, B> {
        Typed {
            sup: self.sup.into(),
            typ: self.typ,
        }
    }
}

impl<'a, T> From<Typed<'a, T>> for (T, Type<'a>) {
    fn from(value: Typed<'a, T>) -> Self {
        (value.sup, value.typ)
    }
}

fn typed<'a, T>(sup: T, typ: Type<'a>) -> Typed<'a, T> {
    Typed { sup, typ }
}

pub fn analyse(ast: Ast) -> Asg {
    Analyse::new().run(ast)
}

#[derive(Clone)]
struct Field<'a> {
    pub id: usize,
    pub typ: Type<'a>,
}

struct Struct<'a> {
    generics: Vec<Generic<'a>>,
    fields: HashMap<&'a str, Field<'a>>,
}

pub struct Info<'a> {
    pub types: Vec<asg::Type<'a>>,
}

struct Analyse<'a> {
    ret_type: Type<'a>,
    break_types: Vec<Type<'a>>,
    ast_structs: HashMap<&'a str, ast::Struct<'a>>,
    type_aliases: HashMap<&'a str, ast::Type<'a>>,
    asg_structs: HashMap<&'a str, asg::Struct<'a>>,
    structs: HashMap<&'a str, Struct<'a>>,
    errors: Vec<CheckError<'a>>,
    context: Context<'a, Type<'a>>,
    type_context: Context<'a, Type<'a>>,
    corrupt: HashSet<&'a str>,
    cold_types: Vec<Type<'a>>,
    types: Vec<Type<'a>>,
    impls: HashMap<&'a str, Vec<Type<'a>>>,
    constraints: HashMap<&'a str, Option<&'a str>>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        Self {
            structs: HashMap::new(),
            ast_structs: HashMap::new(),
            context: Context::new(),
            errors: Vec::new(),
            corrupt: HashSet::new(),
            cold_types: Vec::new(),
            types: Vec::new(),
            type_context: Context::new(),
            asg_structs: HashMap::new(),
            type_aliases: HashMap::new(),
            ret_type: Type::Unknown(None),
            impls: HashMap::new(),
            break_types: Vec::new(),
            constraints: HashMap::new(),
        }
    }

    pub fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        self.ast_structs = ast.structs;
        self.type_aliases = ast.type_aliases;
        self.typ(ast::Type::name(Lame {
            name: "arr",
            location: ast.end,
        }));
        let mut consts = Vec::with_capacity(ast.consts.len());
        for const_ in &ast.consts {
            let typ = self.typ(const_.typ.clone());
            let name = const_.name;
            self.context.insert(name, typ);
        }
        let mut trait_header_names = HashMap::new();
        self.type_context.new_layer();
        self.type_context.insert("self", Type::Generic("self"));
        for (name, trait_) in ast.traits {
            trait_header_names.insert(name, HashSet::new());
            for header in trait_.headers {
                let header_names = trait_header_names.get_mut(name).unwrap();
                header_names.insert(header.lame.name);
                let mut typ = self.fun_type(header.typ);
                typ.generics.insert(
                    0,
                    Generic {
                        name: "self",
                        constraint: Some(name),
                    },
                );
                self.context.insert(header.lame.name, typ.into());
            }
        }
        self.type_context.pop_layer();
        for impl_ in &ast.impls {
            let typ = self.typ(impl_.typ.clone());
            if !self.impls.contains_key(impl_.lame.name) {
                self.impls.insert(impl_.lame.name, Vec::new());
            }
            self.impls.get_mut(impl_.lame.name).unwrap().push(typ);
        }
        for extrn in ast.externs {
            let typ = self.fun_type(extrn.typ);
            self.context.insert(extrn.name, typ.into());
        }
        for fun in &ast.funs {
            let typ = self.fun_type(fun.header.typ.clone()).into();
            self.context.insert(fun.header.lame.name, typ);
        }
        for const_ in ast.consts {
            let location = const_.expr.location();
            let expr = self.const_expr(const_.expr);
            let typ = self.context.get(const_.name).unwrap().clone();
            let typ = self.unify(location, typ, expr.typ);
            let asg_type = self.asg_type(&typ);
            consts.push((const_.name, asg_type, expr.sup));
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|fun| (fun.header.lame.name, self.fun(fun)))
            .collect();
        self.type_context.new_layer();
        let mut trait_funs = HashMap::new();
        for impl_ in ast.impls {
            let typ = self.typ(impl_.typ);
            let asg_type = self.asg_type(&typ);
            self.type_context.insert("self", typ);
            match trait_header_names.get(impl_.lame.name) {
                Some(header_names) => {
                    let mut header_names = header_names.clone();
                    for fun in impl_.funs {
                        if !header_names.remove(fun.header.lame.name) {
                            self.fail(NoMethod {
                                location: fun.header.lame.location,
                                name: fun.header.lame.name,
                                trait_name: impl_.lame.name,
                            });
                            self.fun(fun);
                        } else {
                            let true_header =
                                self.context.get(fun.header.lame.name).unwrap().clone();
                            let mut true_header = self
                                .get_fun_type(true_header, fun.header.lame.location)
                                .unwrap();
                            self.specify_fun_type(&mut true_header);
                            let found = self.fun_type(fun.header.typ.clone()).into();
                            self.unify(fun.header.lame.location, true_header.into(), found);
                            let name = fun.header.lame.name;
                            let fun = self.fun(fun);
                            if !trait_funs.contains_key(name) {
                                trait_funs.insert(name, Vec::new());
                            }
                            trait_funs
                                .get_mut(name)
                                .unwrap()
                                .push((asg_type.clone(), fun));
                        }
                    }
                    if !header_names.is_empty() {
                        self.fail(NotAll {
                            location: impl_.lame.location,
                            kind: "method",
                            rest: header_names.into_iter().collect(),
                        });
                    }
                }
                None => {
                    self.fail(NotDeclared {
                        location: impl_.lame.location,
                        kind: "trait",
                        name: impl_.lame.name,
                    });
                    for fun in impl_.funs {
                        self.fun(fun);
                    }
                }
            }
        }
        self.type_context.pop_layer();
        let info = self.info();
        if !self.errors.is_empty() {
            die(Error(self.errors))
        }
        Asg {
            funs,
            info,
            structs: self.asg_structs,
            trait_funs,
            consts,
        }
    }

    fn const_expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Literal(literal, _) => self.literal(literal).map_into(),
            Expr::New(new) => self.new_expr(new, true),
            expr => {
                self.fail(NotCompTime {
                    location: expr.location(),
                });
                self.fake_expr()
            }
        }
    }

    fn info(&mut self) -> Info<'a> {
        let cold_types = std::mem::take(&mut self.cold_types);
        let types: Vec<_> = cold_types
            .into_iter()
            .map(|typ| self.hot_type(&typ))
            .collect();
        Info { types }
    }

    fn hot_type(&mut self, typ: &Type<'a>) -> asg::Type<'a> {
        self.try_hot_type(typ).unwrap_or(asg::Type::I32)
    }

    fn expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Call(call) => match self.call(call) {
                Ok(call) => call.map_into(),
                Err(_) => self.fake_expr(),
            },
            Expr::Binary(binary) => self.binary(*binary).map_into(),
            Expr::Literal(literal, _) => self.literal(literal).map_into(),
            Expr::Var(name) => self.var(name).map_into(),
            Expr::If(if_expr) => self.if_expr(*if_expr).map_into(),
            Expr::Get(get) => self.get(*get).map_into(),
            Expr::Block(block) => self.block(*block).map_into(),
            Expr::Let(let_expr) => self.let_expr(*let_expr).map_into(),
            Expr::Field(field) => match self.field(*field) {
                Ok(f) => f.map_into(),
                Err(_) => self.fake_expr(),
            },
            Expr::Assign(assign) => self.assign(*assign).map_into(),
            Expr::New(new) => self.new_expr(new, false),
            Expr::Loop(loop_expr) => self.loop_expr(*loop_expr).map_into(),
            Expr::Ref(ref_expr) => self.ref_expr(*ref_expr).map_into(),
            Expr::Return(ret) => self.ret(*ret).map_into(),
            Expr::Cast(cast) => self.cast(*cast).map_into(),
            Expr::Array(array) => self.array(array).map_into(),
            Expr::ImplicitUnit(_) => self.literal(Literal::Unit).map_into(),
            Expr::Break(break_expr) => self.break_expr(*break_expr).map_into(),
        }
    }

    fn array(&mut self, array: Array<'a>) -> Typed<'a, asg::Tuple<'a>> {
        let mut typ = Type::Unknown(None);
        let exprs = array
            .elems
            .into_iter()
            .map(|e| {
                let location = e.location();
                let (e, e_typ) = self.expr(e).into();
                let typ_ = std::mem::take(&mut typ);
                typ = self.unify(location, typ_, e_typ);
                (self.asg_type(&typ), e)
            })
            .collect();
        typed(asg::Tuple { exprs }, Type::Ptr(Box::new(typ)))
    }

    fn cast(&mut self, cast: Cast<'a>) -> Typed<'a, asg::Cast<'a>> {
        let (expr, from_typ) = self.expr(cast.expr).into();
        let to = self.typ(cast.typ);
        let typ = self.cast_typ(cast.location, from_typ.clone(), to);
        let from = self.asg_type(&from_typ);
        let to = self.asg_type(&typ);
        typed(asg::Cast { expr, from, to }, typ)
    }

    fn cast_typ(&mut self, location: Location<'a>, from: Type<'a>, to: Type<'a>) -> Type<'a> {
        match (from, to) {
            (Type::Var(id), to) if self.types[id] == Type::Number && to.is_number() => {
                if DEBUG {
                    eprintln!("{id} = {to}");
                }
                self.types[id] = to;
                Type::Var(id)
            }
            (Type::Var(id), to) => self.cast_typ(location, self.types[id].clone(), to),
            (from, to) if from.is_number() && to.is_number() => to,
            (from, to) => {
                let from = from.clone();
                self.fail(NoCast { location, from, to });
                Type::Error
            }
        }
    }

    fn ret(&mut self, ret: Return<'a>) -> Typed<'a, asg::Return<'a>> {
        let location = ret.expr.location();
        let (expr, typ) = self.expr(ret.expr).into();
        self.unify(location, self.ret_type.clone(), typ);
        typed(asg::Return { expr }, Type::Unreachable)
    }

    fn break_expr(&mut self, break_expr: Break<'a>) -> Typed<'a, asg::Break<'a>> {
        let location = break_expr.expr.location();
        let (expr, expr_typ) = self.expr(break_expr.expr).into();
        let typ = match self.break_types.last_mut() {
            Some(typ) => take(typ),
            None => {
                self.fail(NotInLoop {
                    location: break_expr.location,
                });
                Type::Error
            }
        };
        if !matches!(typ, Type::Error) {
            *self.break_types.last_mut().unwrap() = self.unify(location, typ, expr_typ);
        }
        typed(asg::Break { expr }, Type::Unreachable)
    }

    fn ref_expr(&mut self, ref_expr: Ref<'a>) -> Typed<'a, asg::Ref<'a>> {
        let (expr, typ) = self.expr(ref_expr.expr).into();
        let expr_typ = self.asg_type(&typ);
        typed(asg::Ref { expr, expr_typ }, Type::Ptr(Box::new(typ)))
    }

    fn loop_expr(&mut self, loop_expr: Loop<'a>) -> Typed<'a, asg::Loop<'a>> {
        self.break_types.push(Type::Unreachable);
        let body = self.block(loop_expr.body.into()).sup;
        typed(asg::Loop { body }, self.break_types.pop().unwrap())
    }

    fn new_expr(&mut self, new: New<'a>, is_const: bool) -> Typed<'a, asg::Expr<'a>> {
        let typ = self.typ(ast::Type::name(new.lame));
        if matches!(typ, Type::Error) {
            return self.fake_expr();
        }
        let (name, generics) = match self.type_name(typ) {
            Some(name) => name,
            None => {
                self.fail(NotStruct {
                    location: new.lame.location,
                    name: new.lame.name,
                });
                return self.fake_expr();
            }
        };
        self.type_context.new_layer();
        self.specify_struct_generics(name, &generics);
        let mut exprs: Vec<(asg::Type, asg::Expr)> = Vec::new();
        let mut fields = self.structs[name].fields.clone();
        exprs.resize_with(new.fields.len(), Default::default);
        for field in new.fields {
            let location = field.expr.location();
            let (expr, typ) = if is_const {
                self.const_expr(field.expr)
            } else {
                self.expr(field.expr)
            }
            .into();
            let Field {
                typ: mut field_typ,
                id,
            } = match fields.remove(field.lame.name) {
                Some(field) => field,
                None => return self.fake_expr(),
            };
            self.specify(&mut field_typ);
            let typ = self.unify(location, field_typ, typ);
            let asg_type = self.asg_type(&typ);
            exprs[id] = (asg_type, expr);
        }
        if !fields.is_empty() {
            self.fail(NotAll {
                location: new.lame.location,
                kind: "field",
                rest: fields.into_keys().collect(),
            });
            return self.fake_expr();
        }
        self.type_context.pop_layer();
        typed(Tuple { exprs }.into(), Type::Name(name, generics))
    }

    fn assign(&mut self, assign: Assign<'a>) -> Typed<'a, asg::Assign<'a>> {
        let location = assign.expr.location();
        let (expr, expr_typ) = self.expr(assign.expr).into();
        let (to, to_typ) = self.expr(assign.to).into();
        let needs_deref = self.is_ref(&to_typ) && !self.is_ref(&expr_typ);
        let typ_ = self.unify(location, to_typ, expr_typ);
        let typ = if needs_deref {
            match &typ_ {
                Type::Ref(typ_) => self.asg_type(typ_),
                _ => unreachable!(),
            }
        } else {
            self.asg_type(&typ_)
        };
        typed(
            asg::Assign {
                expr,
                to: if needs_deref {
                    asg::Deref {
                        expr: to,
                        typ: typ.clone(),
                    }
                    .into()
                } else {
                    to
                },
                expr_type: typ,
            },
            typ_,
        )
    }

    fn fake_expr(&self) -> Typed<'a, asg::Expr<'a>> {
        typed(asg::Literal::Int(0, asg::Type::I32).into(), Type::Error)
    }

    fn field(&mut self, field_expr: ast::FieldExpr<'a>) -> Result<Typed<'a, asg::Field<'a>>, Fail> {
        let (expr, typ) = self.expr(field_expr.expr).into();
        if matches!(typ, Type::Error) {
            return Err(Fail);
        }
        let is_ref = self.is_ref(&typ);
        let (name, generics) = self.type_name(typ.clone()).ok_or_else(|| {
            self.fail(NoField {
                location: field_expr.name_location,
                name: field_expr.name,
                typ,
            })
        })?;
        self.type_context.new_layer();
        self.specify_struct_generics(name, &generics);
        let Field { mut typ, id } = match self.structs[name].fields.get(field_expr.name) {
            Some(field) => Ok(field),
            None => Err(self.fail(NoField {
                location: field_expr.name_location,
                name: field_expr.name,
                typ: Type::Name(name, generics),
            })),
        }?
        .clone();
        self.specify(&mut typ);
        let generics = self
            .type_context
            .pop_layer()
            .into_values()
            .map(|typ| self.asg_type(&typ))
            .collect();
        let from_type = asg::Type::Name(name, generics);
        Ok(typed(
            asg::Field {
                from: if is_ref {
                    asg::Deref {
                        expr,
                        typ: from_type.clone(),
                    }
                    .into()
                } else {
                    expr
                },
                id,
                typ: self.asg_type(&typ),
                from_type,
            },
            typ,
        ))
    }

    fn specify_struct_generics(&mut self, name: &str, generics: &Vec<Type<'a>>) {
        for (index, generic) in self.structs[name].generics.iter().enumerate() {
            let id = self.types.len();
            self.types.push(
                generics
                    .get(index)
                    .cloned()
                    .unwrap_or(Type::Unknown(generic.constraint)),
            );
            if DEBUG {
                eprintln!("{id} = {}", self.types[id]);
            }
            self.type_context.insert(generic.name, Type::Var(id));
        }
    }

    fn is_ref(&self, typ: &Type<'a>) -> bool {
        match typ {
            Type::Ref(_) => true,
            Type::Var(id) => self.is_ref(&self.types[*id]),
            _ => false,
        }
    }

    fn type_name(&self, typ: Type<'a>) -> Option<(&'a str, Vec<Type<'a>>)> {
        match typ {
            Type::Name(name, generics) => Some((name, generics)),
            Type::Ref(typ) => self.type_name(*typ),
            Type::Var(id) => self.type_name(self.types[id].clone()),
            _ => None,
        }
    }

    fn fail(&mut self, error: impl Into<CheckError<'a>>) -> Fail {
        self.errors.push(error.into());
        Fail
    }

    fn let_expr(&mut self, let_expr: Let<'a>) -> Typed<'a, asg::Let<'a>> {
        let name = let_expr.name;
        let location = let_expr.expr.location();
        let (expr, mut typ_) = self.expr(let_expr.expr).into();
        if let Some(typ) = let_expr.typ {
            let typ = self.typ(typ);
            typ_ = self.unify(location, typ, typ_);
        }
        let typ = self.asg_type(&typ_);
        if DEBUG {
            eprintln!("{name} = {typ_}");
        }
        self.context.insert(name, typ_);
        let name = let_expr.name;
        typed(asg::Let { name, expr, typ }, Prime::Unit.into())
    }

    fn block(&mut self, block: Block<'a>) -> Typed<'a, asg::Block<'a>> {
        let stmts: Vec<_> = block.stmts.into_iter().map(|e| self.expr(e).sup).collect();
        let (ret, typ) = self.expr(block.ret).into();
        typed(asg::Block { stmts, ret }, typ)
    }

    fn get(&mut self, get: Get<'a>) -> Typed<'a, asg::Deref<'a>> {
        let location = get.from.location();
        let from = self.expr(get.from);
        let typ = self.get_type(from.typ, location);
        let asg_type = self.asg_type(&typ);
        let location = get.index.location();
        let index = self.expr(get.index);
        self.unify(location, Prime::U64.into(), index.typ);
        let res = asg::Deref {
            expr: asg::Binary {
                left: from.sup,
                op: asg::BinOp::Add,
                right: asg::Binary {
                    left: index.sup,
                    op: asg::BinOp::Multiply,
                    right: asg::Literal::SizeOf(asg_type).into(),
                    typ: asg::Type::U64,
                }
                .into(),
                typ: asg::Type::U64,
            }
            .into(),
            typ: self.asg_type(&typ),
        };
        typed(res, typ)
    }

    fn unify(&mut self, location: Location<'a>, expected: Type<'a>, found: Type<'a>) -> Type<'a> {
        self.try_unify(location, expected, found)
            .unwrap_or_else(|e| {
                self.fail(*e);
                Type::Error
            })
    }

    fn unify_ptrs(
        &mut self,
        location: Location<'a>,
        mut expected: Box<Type<'a>>,
        found: Type<'a>,
    ) -> Result<Type<'a>, Box<WrongType<'a>>> {
        *expected = self
            .try_unify(location, *expected, found)
            .map_err(|mut e| {
                e.expected = Type::Ptr(Box::new(e.expected));
                e.found = Type::Ptr(Box::new(e.found));
                e
            })?;
        Ok(Type::Ptr(expected))
    }

    fn unify_refs(
        &mut self,
        location: Location<'a>,
        mut expected: Box<Type<'a>>,
        found: Type<'a>,
    ) -> Result<Type<'a>, Box<WrongType<'a>>> {
        *expected = self
            .try_unify(location, *expected, found)
            .map_err(|mut e| {
                e.expected = Type::Ref(Box::new(e.expected));
                e.found = Type::Ref(Box::new(e.found));
                e
            })?;
        Ok(Type::Ref(expected))
    }

    fn try_unify(
        &mut self,
        location: Location<'a>,
        expected: Type<'a>,
        found: Type<'a>,
    ) -> Result<Type<'a>, Box<WrongType<'a>>> {
        match (expected, found) {
            (a, b) if a == b => Ok(a),
            (Type::Var(id), b) => {
                let a = std::mem::take(&mut self.types[id]);
                self.types[id] = self.try_unify(location, a, b)?;
                if DEBUG {
                    eprintln!("{id} = {}", self.types[id]);
                }
                Ok(Type::Var(id))
            }
            (a, Type::Var(id)) => {
                let b = std::mem::take(&mut self.types[id]);
                self.types[id] = self.try_unify(location, a, b)?;
                if DEBUG {
                    eprintln!("{id} = {}", self.types[id]);
                }
                Ok(Type::Var(id))
            }
            (_, Type::Error) => Ok(Type::Error),
            (Type::Error, _) => Ok(Type::Error),
            (Type::Unknown(None), b) => Ok(b),
            (a, Type::Unknown(None)) => Ok(a),
            (Type::Unknown(Some(a)), Type::Generic(b))
                if self.constraints[b].is_some_and(|b| a == b) =>
            {
                Ok(Type::Generic(b))
            }
            (Type::Unknown(Some(name)), typ) => Ok(self.constrain(location, name, typ)),
            (a, Type::Unreachable) => Ok(a),
            (Type::Unreachable, b) => Ok(b),
            (Type::Ptr(expected), Type::Ptr(found)) => self.unify_ptrs(location, expected, *found),
            (Type::Ref(expected), Type::Ref(found)) => self.unify_refs(location, expected, *found),
            (Type::Ref(mut expected), found) => {
                *expected = self.try_unify(location, *expected, found)?;
                Ok(Type::Ref(expected))
            }
            (Type::Fun(mut a), Type::Fun(mut b)) if a.params.len() == b.params.len() => {
                let len = a.params.len();
                for i in 0..len {
                    a.params[i] = match self.try_unify(
                        location,
                        take(&mut a.params[i]),
                        take(&mut b.params[i]),
                    ) {
                        Ok(typ) => typ,
                        Err(mut err) => {
                            for j in 0..len {
                                if j != i {
                                    a.params[j] = Type::Error;
                                    b.params[j] = Type::Error;
                                }
                            }
                            a.params[i] = err.expected;
                            b.params[i] = err.found;
                            err.expected = Type::Fun(a);
                            err.found = Type::Fun(b);
                            return Err(err);
                        }
                    };
                }
                a.ret_type =
                    match self.try_unify(location, take(&mut a.ret_type), take(&mut b.ret_type)) {
                        Ok(typ) => typ,
                        Err(mut e) => {
                            a.ret_type = e.expected;
                            b.ret_type = e.found;
                            e.expected = Type::Fun(a);
                            e.found = Type::Fun(b);
                            return Err(e);
                        }
                    };
                Ok(Type::Fun(a))
            }
            (Type::Name(a, mut ga), Type::Name(b, mut gb)) if a == b && ga.len() == gb.len() => {
                let len = ga.len();
                for i in 0..len {
                    ga[i] = match self.try_unify(location, ga[i].clone(), gb[i].clone()) {
                        Ok(typ) => typ,
                        Err(mut err) => {
                            for j in 0..len {
                                if j != i {
                                    ga[j] = Type::Error;
                                    gb[j] = Type::Error;
                                }
                            }
                            ga[i] = err.expected;
                            gb[i] = err.found;
                            err.expected = Type::Name(a, ga);
                            err.found = Type::Name(b, gb);
                            return Err(err);
                        }
                    };
                }
                Ok(Type::Name(a, ga))
            }
            (a, Type::Number) if a.is_number() => Ok(a),
            (Type::Number, b) if b.is_number() => Ok(b),
            (mut expected, mut found) => {
                self.reveal(&mut expected);
                self.reveal(&mut found);
                Err(Box::new(WrongType {
                    location,
                    expected,
                    found,
                }))
            }
        }
    }

    fn constrain(&mut self, location: Location<'a>, name: &'a str, typ: Type<'a>) -> Type<'a> {
        let mut ttyp = typ.clone();
        self.reveal(&mut ttyp);
        for impl_typ in self.impls.get(name).cloned().into_iter().flatten() {
            if self.try_unify(location, impl_typ, ttyp.clone()).is_ok() {
                return typ;
            }
        }
        self.fail(NotImpl {
            location,
            typ,
            name,
            types: self.impls.get(name).cloned().unwrap_or_default(),
        });
        Type::Error
    }

    fn reveal(&mut self, typ: &mut Type<'a>) {
        match typ {
            Type::Ptr(typ) => self.reveal(typ),
            Type::Name(_, items) => {
                for typ in items {
                    self.reveal(typ)
                }
            }
            Type::Fun(fun_type) => {
                for typ in &mut fun_type.params {
                    self.reveal(typ);
                }
                self.reveal(&mut fun_type.ret_type);
            }
            Type::Prime(_) => {}
            Type::Error => {}
            Type::Number => {}
            Type::Var(id) => *typ = self.types[*id].clone(),
            Type::Unreachable => {}
            Type::Generic(_) => {}
            Type::Unknown(_) => {}
            Type::Ref(typ) => self.reveal(typ),
        }
    }

    fn get_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
            Type::Var(id) => self.get_type(self.types[id].clone(), location),
            Type::Ptr(typ) => *typ,
            _ => {
                self.errors.push(NoIndex { location, typ }.into());
                Type::Error
            }
        }
    }

    fn if_expr(&mut self, if_expr: If<'a>) -> Typed<'a, asg::If<'a>> {
        let location = if_expr.condition.location();
        let (condition, typ) = self.expr(if_expr.condition).into();
        self.unify(location, Prime::Bool.into(), typ);
        let then_location = if_expr.then_expr.location();
        let (then_expr, mut then_typ) = self.expr(if_expr.then_expr).into();
        let mut location = if_expr.else_expr.location();
        let (else_expr, mut else_typ) = self.expr(if_expr.else_expr).into();
        if matches!(else_typ, Type::Prime(Prime::Unit))
            && matches!(else_expr, asg::Expr::Literal(_))
        {
            std::mem::swap(&mut then_typ, &mut else_typ);
            location = then_location;
        }
        let typ = self.unify(location, then_typ, else_typ);
        let res = asg::If {
            condition,
            then_expr,
            else_expr,
        };
        typed(res, typ)
    }

    fn call(&mut self, call: Call<'a>) -> Result<Typed<'a, asg::Call<'a>>, Fail> {
        let name = call.lame.name;
        let location = call.lame.location;
        let typ = self.var(call.lame).typ;
        let args: Vec<_> = call
            .args
            .into_iter()
            .map(|e| (e.location(), self.expr(e)))
            .collect();
        let mut typ = match self.get_fun_type(typ, location) {
            Ok(typ) => typ,
            Err(_) => {
                for (location, expr) in args.into_iter() {
                    self.unify(location, Type::Error, expr.typ);
                }
                return Err(Fail);
            }
        };
        self.type_context.new_layer();
        for generic in &typ.generics {
            let typ = self.new_type_var(Type::Unknown(generic.constraint));
            self.type_context.insert(generic.name, typ);
        }
        self.specify_fun_type(&mut typ);
        let generics = typ
            .generics
            .iter()
            .map(|g| {
                let typ = self
                    .type_context
                    .sup
                    .last_mut()
                    .unwrap()
                    .remove(g.name)
                    .unwrap();
                (g.name, self.asg_type(&typ))
            })
            .collect();
        self.type_context.pop_layer();
        let args = args
            .into_iter()
            .zip(typ.params)
            .map(|((location, expr), expected)| {
                let typ = self.unify(location, expected, expr.typ);
                let asg_type = self.asg_type(&typ);
                (asg_type, expr.sup)
            })
            .collect();
        let ret_type = self.asg_type(&typ.ret_type);
        Ok(typed(
            asg::Call {
                name,
                args,
                generics,
                ret_type,
            },
            typ.ret_type,
        ))
    }

    fn specify_fun_type(&mut self, fun_type: &mut FunType<'a>) {
        for param in &mut fun_type.params {
            self.specify(param);
        }
        self.specify(&mut fun_type.ret_type);
    }

    fn specify(&mut self, typ: &mut Type<'a>) {
        match typ {
            Type::Ptr(typ) => self.specify(typ),
            Type::Name(_, generics) => {
                for typ in generics {
                    self.specify(typ);
                }
            }
            Type::Fun(fun_type) => {
                self.type_context.new_layer();
                for generic in &fun_type.generics {
                    self.type_context
                        .insert(generic.name, Type::Generic(generic.name));
                }
                self.specify_fun_type(fun_type);
                self.type_context.pop_layer();
            }
            Type::Prime(_) => {}
            Type::Error => {}
            Type::Number => {}
            Type::Var(id) => {
                let mut typ = std::mem::take(&mut self.types[*id]);
                self.specify(&mut typ);
                self.types[*id] = typ;
                if DEBUG {
                    eprintln!("{id} = {}", self.types[*id]);
                }
            }
            Type::Unreachable => {}
            Type::Generic(g) => {
                if let Some(t) = self.type_context.get(g) {
                    *typ = t.clone()
                }
            }
            Type::Unknown(_) => {}
            Type::Ref(typ) => self.specify(typ),
        }
    }

    fn get_fun_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Result<FunType<'a>, Fail> {
        match typ {
            Type::Error => Err(Fail),
            Type::Fun(typ) => Ok(*typ),
            _ => Err(self.fail(NoCall { location, typ })),
        }
    }

    fn var(&mut self, var: Lame<'a>) -> Typed<'a, &'a str> {
        match self.context.get(var.name) {
            Some(typ) => typed(var.name, typ.clone()),
            None => {
                if !self.corrupt.contains(var.name) {
                    self.corrupt.insert(var.name);
                    self.fail(NotDeclared {
                        location: var.location,
                        kind: "item",
                        name: var.name,
                    });
                }
                typed(var.name, Type::Error)
            }
        }
    }

    fn binary(&mut self, binary: Binary<'a>) -> Typed<'a, asg::Binary<'a>> {
        let (left, left_typ) = self.expr(binary.left).into();
        let right_location = binary.right.location();
        let (right, right_typ) = self.expr(binary.right).into();
        let op = self.bin_op(binary.op);
        let typ = match binary.op {
            BinOp::Plus
            | BinOp::Mod
            | BinOp::Div
            | BinOp::And
            | BinOp::Subtract
            | BinOp::Multiply => {
                if matches!(left_typ, Type::Ptr(_)) {
                    self.unify(right_location, Prime::U64.into(), right_typ);
                    left_typ
                } else {
                    self.unify(right_location, left_typ, right_typ)
                }
            }
            BinOp::Equal | BinOp::NotEqual | BinOp::Less => {
                self.unify(right_location, left_typ, right_typ);
                Prime::Bool.into()
            }
        };
        let asg_type = self.asg_type(&typ);
        let expr = match &typ {
            Type::Ptr(typ) => asg::Binary {
                left,
                op,
                right: asg::Binary {
                    left: right,
                    op: asg::BinOp::Multiply,
                    right: asg::Literal::SizeOf(self.asg_type(typ)).into(),
                    typ: asg::Type::U64,
                }
                .into(),
                typ: asg_type,
            },
            _ => asg::Binary {
                left,
                op,
                right,
                typ: asg_type,
            },
        };
        typed(expr, typ)
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
            BinOp::NotEqual => asg::BinOp::NotEqual,
            BinOp::Mod => asg::BinOp::Modulo,
            BinOp::Div => asg::BinOp::Divide,
            BinOp::And => asg::BinOp::And,
            BinOp::Subtract => asg::BinOp::Subtract,
            BinOp::Multiply => asg::BinOp::Multiply,
        }
    }

    fn literal(&mut self, literal: Literal<'a>) -> Typed<'a, asg::Literal<'a>> {
        match literal {
            Literal::Unit => typed(asg::Literal::Int(0, asg::Type::I32), Prime::Unit.into()),
            Literal::Int(i) => {
                let typ = self.new_type_var(Type::Number);
                let asg_type = self.asg_type(&typ);
                typed(asg::Literal::Int(i, asg_type), typ)
            }
            Literal::Str(s) => typed(
                asg::Literal::Str(s),
                Type::Name("arr", vec![Prime::U8.into()]),
            ),
            Literal::Bool(b) => typed(
                asg::Literal::Int(b as i64, asg::Type::U8),
                Prime::Bool.into(),
            ),
            Literal::Size(typ) => {
                let typ = self.typ(typ);
                typed(asg::Literal::SizeOf(self.asg_type(&typ)), typ)
            }
        }
    }

    fn new_type_var(&mut self, typ: Type<'a>) -> Type<'a> {
        let id = self.types.len();
        self.types.push(typ);
        if DEBUG {
            eprintln!("{id} = {}", self.types[id]);
        }
        Type::Var(id)
    }

    fn lame_type(&mut self, lame: Lame<'a>, generics: Vec<ast::Type<'a>>) -> Type<'a> {
        let mut generics: Vec<_> = generics.into_iter().map(|t| self.typ(t)).collect();
        if let Some(typ) = self.type_context.get(lame.name) {
            typ.clone()
        } else if let Some(typ) = self.type_aliases.remove(lame.name) {
            let typ = self.typ(typ);
            self.type_context.sup[0].insert(lame.name, typ.clone());
            typ
        } else if let Some(struct_) = self.structs.get(lame.name) {
            if generics.is_empty() {
                generics = struct_
                    .generics
                    .iter()
                    .map(|g| Type::Generic(g.name))
                    .collect();
            }
            Type::Name(lame.name, generics)
        } else if self.corrupt.contains(lame.name) {
            Type::Error
        } else if let Some(r#struct) = self.ast_structs.remove(lame.name) {
            self.r#struct(lame.name, r#struct);
            Type::Name(lame.name, generics)
        } else {
            self.corrupt.insert(lame.name);
            self.errors.push(
                NotDeclared {
                    location: lame.location,
                    kind: "type item",
                    name: lame.name,
                }
                .into(),
            );
            Type::Error
        }
    }

    fn r#struct(&mut self, name: &'a str, r#struct: ast::Struct<'a>) {
        let mut fields = HashMap::new();
        let mut asg_fields = Vec::new();
        self.type_context.new_layer();
        for generic in &r#struct.generics {
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
        }
        for (id, field) in r#struct.fields.into_iter().enumerate() {
            let typ = self.typ(field.typ);
            asg_fields.push(self.asg_type(&typ));
            fields.insert(field.name, Field { id, typ });
        }
        self.type_context.pop_layer();
        let generics = r#struct.generics;
        let r#struct = Struct { fields, generics };
        let asg_struct = asg::Struct { fields: asg_fields };
        self.structs.insert(name, r#struct);
        self.asg_structs.insert(name, asg_struct);
    }

    fn typ(&mut self, typ: ast::Type<'a>) -> Type<'a> {
        match typ {
            ast::Type::Ptr(t, _) => Type::Ptr(Box::new(self.typ(*t))),
            ast::Type::Name(lame, generics) => self.lame_type(lame, generics),
            ast::Type::Fun(fun_type) => self.fun_type(*fun_type).into(),
            ast::Type::Prime(prime, _) => Type::Prime(prime),
            ast::Type::Ref(t, _) => Type::Ref(Box::new(self.typ(*t))),
        }
    }

    fn fun_type(&mut self, fun_type: ast::FunType<'a>) -> FunType<'a> {
        self.type_context.new_layer();
        for generic in &fun_type.generics {
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
        }
        let res = FunType {
            generics: fun_type.generics,
            params: fun_type.params.into_iter().map(|t| self.typ(t)).collect(),
            ret_type: self.typ(fun_type.ret),
        };
        self.type_context.pop_layer();
        res
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        self.context.new_layer();
        self.type_context.new_layer();
        for generic in fun.header.typ.generics {
            self.constraints.insert(generic.name, generic.constraint);
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
        }
        let params = fun
            .header
            .params
            .iter()
            .zip(fun.header.typ.params)
            .map(|(param, typ)| {
                let typ_ = self.typ(typ);
                let typ = self.asg_type(&typ_);
                self.context.insert(param, typ_);
                (*param, typ)
            })
            .collect();
        let ret_typ = self.typ(fun.header.typ.ret);
        let ret_type = self.asg_type(&ret_typ);
        self.ret_type = ret_typ;
        let location = fun.body.location();
        let last_semi_location = if let Expr::Block(block) = &fun.body
            && matches!(block.ret, Expr::ImplicitUnit(_))
        {
            block.last_semi_location
        } else {
            None
        };
        let (body, typ) = self.expr(fun.body).into();
        let is_unit = matches!(typ, Type::Prime(Prime::Unit));
        let typ = self.unify(location, self.ret_type.clone(), typ);
        if let Some(location) = last_semi_location
            && matches!(typ, Type::Error)
            && is_unit
        {
            self.errors.last_mut().unwrap().help = Some(SemicolonEndBlock { location }.into());
        }
        self.context.pop_layer();
        self.type_context.pop_layer();
        asg::Fun {
            params,
            body,
            ret_type,
        }
    }

    fn asg_type(&mut self, typ: &Type<'a>) -> asg::Type<'a> {
        match self.try_hot_type(typ) {
            Some(typ) => typ,
            None => self.new_cold_type(typ.clone()),
        }
    }

    fn try_hot_type(&self, typ: &Type<'a>) -> Option<asg::Type<'a>> {
        match typ {
            Type::Ptr(_) => Some(asg::Type::U64),
            Type::Name(name, generics) => Some(asg::Type::Name(
                name,
                generics
                    .iter()
                    .map(|t| self.try_hot_type(t))
                    .collect::<Option<_>>()?,
            )),
            Type::Fun(_) => Some(asg::Type::U64),
            Type::Prime(prime) => Some(self.asg_prime(prime)),
            Type::Error => Some(asg::Type::I32),
            Type::Number => None,
            Type::Var(id) => self.try_hot_type(&self.types[*id]),
            Type::Unreachable => Some(asg::Type::I32),
            Type::Generic(g) => Some(asg::Type::Generic(g)),
            Type::Unknown(_) => None,
            Type::Ref(_) => Some(asg::Type::U64),
        }
    }

    fn new_cold_type(&mut self, typ: Type<'a>) -> asg::Type<'a> {
        let id = self.cold_types.len();
        self.cold_types.push(typ);
        asg::Type::Cold(id)
    }

    fn asg_prime(&self, prime: &Prime) -> asg::Type<'a> {
        match prime {
            Prime::Unit => asg::Type::I32,
            Prime::Bool => asg::Type::U8,
            Prime::I32 => asg::Type::I32,
            Prime::U8 => asg::Type::U8,
            Prime::U64 => asg::Type::U64,
            Prime::I64 => asg::Type::I64,
            Prime::F32 => asg::Type::F32,
            Prime::F64 => asg::Type::F64,
        }
    }
}
