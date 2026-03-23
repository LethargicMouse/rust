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
            CantMatch, CheckError, CheckErrorKind, Fail, NoCast, NoField, NoMethod, NoOp, NotAll,
            NotCompTime, NotDeclared, NotImpl, NotInLoop, NotStruct, Redeclared, SemicolonEndBlock,
            ShouldKnowType, WrongCount, WrongType,
        },
        asg::{self},
        ast::{self, *},
    },
};

pub const DEBUG: bool = false;

#[derive(Clone, PartialEq, Debug)]
struct Constraint<'a> {
    pub name: &'a str,
    pub generics: Vec<Type<'a>>,
}

impl Display for Constraint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if self.generics.is_empty() {
            Ok(())
        } else {
            write!(f, "<{}>", Sep(", ", &self.generics))
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Generic<'a> {
    name: &'a str,
    constraint: Option<Constraint<'a>>,
}

impl<'a> From<&'a str> for Generic<'a> {
    fn from(name: &'a str) -> Self {
        Self {
            constraint: None,
            name,
        }
    }
}

impl Display for Generic<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        match &self.constraint {
            Some(constraint) => write!(f, ": {constraint}"),
            None => Ok(()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct FunType<'a> {
    generics: Vec<Generic<'a>>,
    params: Vec<Type<'a>>,
    ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn")?;
        if !self.generics.is_empty() {
            write!(f, "<{}>", Sep(", ", &self.generics))?;
        }
        write!(f, "({})", Sep(", ", &self.params))?;
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
    Unknown,
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
            Type::Name("slice", gs) if gs.len() == 1 => write!(f, "[{}]", gs[0]),
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
            Type::Var(id) => write!(f, "#{id}"),
            Type::Unreachable => write!(f, "<unreachable>"),
            Type::Generic(n) => write!(f, "{n}"),
            Type::Unknown => write!(f, "_"),
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

    fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::Ref(_))
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

#[derive(Clone)]
#[allow(dead_code)]
struct Impl<'a> {
    generics: Vec<Generic<'a>>,
    trait_generics: Vec<Type<'a>>,
    typ: Type<'a>,
}

struct Analyse<'a> {
    constraints: Vec<(Location<'a>, Type<'a>, Constraint<'a>)>,
    g_constraints: Context<'a, Constraint<'a>>,
    traits: HashMap<&'a str, usize>, // for now its generics cnt
    ret_type: Type<'a>,
    break_types: Vec<Type<'a>>,
    ast_structs: HashMap<&'a str, ast::Struct<'a>>,
    type_aliases: HashMap<&'a str, ast::Type<'a>>,
    asg_structs: HashMap<&'a str, asg::Struct<'a>>,
    structs: HashMap<&'a str, Struct<'a>>,
    enums: HashMap<&'a str, Vec<Generic<'a>>>,
    errors: Vec<CheckError<'a>>,
    context: Context<'a, (Type<'a>, Location<'a>)>,
    type_context: Context<'a, Type<'a>>,
    corrupt: HashSet<&'a str>,
    cold_types: Vec<(Location<'a>, Type<'a>)>,
    types: Vec<Type<'a>>,
    impls: HashMap<&'a str, Vec<Impl<'a>>>,
    labels: HashMap<&'a str, (&'a str, u8, Option<Type<'a>>)>,
    global_funs: HashMap<&'a str, (FunType<'a>, Location<'a>)>,
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
            ret_type: Type::Unknown,
            impls: HashMap::new(),
            break_types: Vec::new(),
            labels: HashMap::new(),
            enums: HashMap::new(),
            global_funs: HashMap::new(),
            traits: HashMap::new(),
            constraints: Vec::new(),
            g_constraints: Context::new(),
        }
    }

    fn gen_slice(&mut self) {
        let fields = [
            (
                "ptr",
                Field {
                    id: 0,
                    typ: Type::Ptr(Box::new(Type::Generic("t"))),
                },
            ),
            (
                "size",
                Field {
                    id: 1,
                    typ: Prime::U64.into(),
                },
            ),
        ]
        .into();
        let generics = [Generic {
            name: "t",
            constraint: None,
        }]
        .into();
        let asg_fields = [
            asg::Type::Ptr(Box::new(asg::Type::Generic("t"))),
            asg::Type::U64,
        ]
        .into();
        let asg_generics = ["t"].into();
        let struct_ = Struct { fields, generics };
        let asg_struct = asg::Struct {
            variants: vec![asg_fields],
            generics: asg_generics,
        };
        self.structs.insert("slice", struct_);
        self.asg_structs.insert("slice", asg_struct);
    }

    pub fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        self.ast_structs = ast.structs;
        self.type_aliases = ast.type_aliases;
        self.gen_slice();
        self.traits = ast
            .traits
            .iter()
            .map(|(n, t)| (*n, t.generics.len()))
            .collect();
        let mut enum_variants = Vec::with_capacity(ast.enums.len());
        for (name, enum_) in ast.enums {
            enum_variants.push((name, enum_.variants));
            let generics = enum_
                .generics
                .into_iter()
                .map(|g| self.generic(&g))
                .collect();
            self.enums.insert(name, generics);
        }
        for (name, variants) in enum_variants {
            self.enum_(name, variants);
        }
        let mut consts = HashMap::with_capacity(ast.consts.len());
        for const_ in &ast.consts {
            let typ = self.typ(&const_.typ);
            self.context
                .insert(const_.lame.name, (typ, const_.lame.location));
        }
        let mut trait_header_names = HashMap::new();
        let mut trait_generics = HashMap::new();
        for (name, trait_) in ast.traits {
            self.type_context.new_layer();
            self.type_context.insert("self", Type::Generic("self"));
            for g in &trait_.generics {
                self.type_context.insert(g.name, Type::Generic(g.name));
            }
            trait_header_names.insert(name, HashSet::new());
            for header in trait_.headers {
                let header_names = trait_header_names.get_mut(name).unwrap();
                header_names.insert(header.lame.name);
                let mut typ = self.fun_type(&header.typ);
                typ.generics.insert(
                    0,
                    Generic {
                        name: "self",
                        constraint: Some(Constraint {
                            name,
                            generics: trait_
                                .generics
                                .iter()
                                .map(|g| Type::Generic(g.name))
                                .collect(),
                        }),
                    },
                );
                for g in &trait_.generics {
                    typ.generics.insert(0, self.generic(g));
                }
                if DEBUG {
                    eprintln!("> new global fun `{}`: {}", header.lame.name, typ)
                }
                self.global_funs
                    .insert(header.lame.name, (typ, header.lame.location));
            }
            trait_generics.insert(name, trait_.generics);
            self.type_context.pop_layer();
        }
        for impl_ in &ast.impls {
            self.type_context.new_layer();
            for g in &impl_.generics {
                self.type_context.insert(g.name, Type::Generic(g.name));
            }
            let trait_generics = impl_.trait_generics.iter().map(|t| self.typ(t)).collect();
            let typ = self.typ(&impl_.typ);
            self.type_context.pop_layer();
            if !self.impls.contains_key(impl_.lame.name) {
                self.impls.insert(impl_.lame.name, Vec::new());
            }
            let generics = impl_.generics.iter().map(|g| self.generic(g)).collect();
            self.impls.get_mut(impl_.lame.name).unwrap().push(Impl {
                generics,
                trait_generics,
                typ,
            });
        }
        for extrn in ast.externs {
            let typ = self.fun_type(&extrn.typ);
            self.global_funs
                .insert(extrn.lame.name, (typ, extrn.lame.location));
        }
        for fun in &ast.funs {
            let typ = self.fun_type(&fun.header.typ);
            if let Some((_, location)) = self.global_funs.get(fun.header.lame.name) {
                self.corrupt.insert(fun.header.lame.name);
                self.fail(Redeclared {
                    location: fun.header.lame.location,
                    kind: "item",
                    name: fun.header.lame.name,
                    other: *location,
                });
            } else {
                self.global_funs
                    .insert(fun.header.lame.name, (typ, fun.header.lame.location));
            }
        }
        for const_ in ast.consts {
            let location = const_.expr.location();
            if DEBUG {
                eprintln!("> const {}", const_.lame.name)
            }
            let expr = self.const_expr(const_.expr);
            let typ = self.context.get(const_.lame.name).unwrap().0.clone();
            let typ = self.unify(location, typ, expr.typ);
            let asg_type = self.asg_type(location, &typ);
            consts.insert(const_.lame.name, (asg_type, expr.sup));
        }
        let funs: HashMap<_, _> = ast
            .funs
            .into_iter()
            .map(|fun| (fun.header.lame.name, self.fun(fun)))
            .collect();
        if !funs.contains_key("main") {
            self.fail(NotDeclared {
                location: ast.end,
                kind: "function",
                name: "main",
            });
        }
        let mut trait_funs = HashMap::new();
        for impl_ in ast.impls {
            self.type_context.new_layer();
            for g in &impl_.generics {
                self.type_context.insert(g.name, Type::Generic(g.name));
            }
            let header_names = match trait_header_names.get(impl_.lame.name) {
                Some(res) => res,
                None => {
                    self.fail(NotDeclared {
                        location: impl_.lame.location,
                        kind: "trait",
                        name: impl_.lame.name,
                    });
                    for fun in impl_.funs {
                        self.fun(fun);
                    }
                    continue;
                }
            };
            let mut asg_trait_generics = Vec::with_capacity(impl_.trait_generics.len());
            let len = impl_.trait_generics.len();
            for (typ, g) in impl_
                .trait_generics
                .into_iter()
                .zip(&trait_generics[impl_.lame.name])
            {
                let typ = self.typ(&typ);
                asg_trait_generics.push(self.asg_type(impl_.lame.location, &typ));
                self.type_context.insert(g.name, typ);
            }
            let location = impl_.typ.location();
            let typ = self.typ(&impl_.typ);
            let asg_type = self.asg_type(location, &typ);
            self.type_context.insert("self", typ);
            let mut header_names = header_names.clone();
            for fun in impl_.funs {
                if !header_names.remove(fun.header.lame.name) {
                    self.fail(NoMethod {
                        location: fun.header.lame.location,
                        name: fun.header.lame.name,
                        trait_name: impl_.lame.name,
                    });
                    self.fun(fun);
                    continue;
                }
                let mut true_header = self
                    .global_funs
                    .get(fun.header.lame.name)
                    .unwrap()
                    .0
                    .clone();
                true_header
                    .generics
                    .drain(0..self.traits[impl_.lame.name] + 1);
                self.specify_fun_type(&mut true_header);
                let found = self.fun_type(&fun.header.typ);
                if true_header != found {
                    self.fail(WrongType {
                        location: fun.header.lame.location,
                        expected: true_header.into(),
                        found: found.into(),
                    });
                }
                let name = fun.header.lame.name;
                let fun = self.fun(fun);
                if !trait_funs.contains_key(name) {
                    trait_funs.insert(name, (len, Vec::new()));
                }
                trait_funs.get_mut(name).unwrap().1.push(asg::Impl {
                    generics: impl_.generics.iter().map(|g| g.name).collect(),
                    trait_generics: asg_trait_generics.clone(),
                    typ: asg_type.clone(),
                    fun,
                });
            }
            if !header_names.is_empty() {
                self.fail(NotAll {
                    location: impl_.lame.location,
                    kind: "method",
                    rest: header_names.into_iter().collect(),
                });
            }

            self.type_context.pop_layer();
        }
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

    fn constrain(&mut self, location: Location<'a>, typ: Type<'a>, mut constraint: Constraint<'a>) {
        if DEBUG {
            eprintln!("> constrain {typ}: {constraint}");
        }
        let mut ttyp = typ.clone();
        self.reveal(&mut ttyp);
        if let Type::Generic(name) = &ttyp
            && let Some(c) = self.g_constraints.get(name)
            && &constraint == c
        {
            return;
        }
        let mut c_generics = constraint.generics.clone();
        for typ in &mut c_generics {
            self.reveal(typ);
        }
        self.impls.entry(constraint.name).or_default();
        for mut impl_ in self.impls[constraint.name].clone() {
            if DEBUG {
                eprintln!("> candidate {}", impl_.typ);
            }
            self.type_context.new_layer();
            for generic in impl_.generics {
                let typ = self.new_type_var(Type::Unknown);
                self.type_context.insert(generic.name, typ);
            }
            self.specify(&mut impl_.typ);
            if self
                .try_unify(location, impl_.typ.clone(), ttyp.clone())
                .is_err()
            {
                self.type_context.pop_layer();
                continue;
            }
            let mut f = true;
            for (typ, ctyp) in impl_
                .trait_generics
                .iter_mut()
                .zip(c_generics.iter().cloned())
            {
                self.specify(typ);
                if self.try_unify(location, typ.clone(), ctyp).is_err() {
                    f = false;
                    break;
                }
            }
            self.type_context.pop_layer();
            if f {
                self.unify(location, impl_.typ, typ);
                for (typ, ctyp) in impl_.trait_generics.into_iter().zip(constraint.generics) {
                    self.unify(location, typ, ctyp);
                }
                return;
            }
        }
        let typ = ttyp;
        constraint.generics = c_generics;
        self.fail(NotImpl {
            location,
            typ,
            constraint,
        });
    }

    fn generic(&mut self, generic: &ast::Generic<'a>) -> Generic<'a> {
        Generic {
            name: generic.name,
            constraint: generic.constraint.as_ref().and_then(|c| self.constraint(c)),
        }
    }

    fn constraint(&mut self, constraint: &ast::Constraint<'a>) -> Option<Constraint<'a>> {
        if !self.traits.contains_key(constraint.lame.name) {
            self.fail(NotDeclared {
                location: constraint.lame.location,
                kind: "trait",
                name: constraint.lame.name,
            });
            return None;
        }
        Some(Constraint {
            name: constraint.lame.name,
            generics: constraint.generics.iter().map(|t| self.typ(t)).collect(),
        })
    }

    fn const_expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Literal(literal, location) => self.literal(location, literal).map_into(),
            Expr::New(new) => self.new_expr(new, true),
            Expr::Var(lame) => self.var(lame),
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
            .map(|(location, typ)| self.hot_type(location, &typ))
            .collect();
        Info { types }
    }

    fn hot_type(&mut self, location: Location<'a>, typ: &Type<'a>) -> asg::Type<'a> {
        self.try_hot_type(typ).unwrap_or_else(|| {
            let mut typ = typ.clone();
            self.reveal(&mut typ);
            self.fail(ShouldKnowType { location, typ });
            asg::Type::Unknown
        })
    }

    fn expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Call(call) => self.call(call),
            Expr::Binary(binary) => self.binary(*binary),
            Expr::Literal(literal, location) => self.literal(location, literal).map_into(),
            Expr::Var(name) => self.var(name),
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
            Expr::ImplicitUnit(location) => self.literal(location, Literal::Unit).map_into(),
            Expr::Break(break_expr) => self.break_expr(*break_expr).map_into(),
            Expr::Negate(negate) => self.negate(*negate).map_into(),
            Expr::Match(match_expr) => self.match_expr(*match_expr),
            Expr::For(for_expr) => self.block(for_expr.desugar()).map_into(),
            Expr::Deref(deref) => self.deref(*deref).map_into(),
        }
    }

    fn match_expr(&mut self, match_expr: Match<'a>) -> Typed<'a, asg::Expr<'a>> {
        let mut res_typ = Type::Unknown;
        let location = match_expr.expr.location();
        let (expr, mut typ) = self.expr(match_expr.expr).into();
        let (name, generics) = match self.type_name(typ.clone()) {
            Some(tn) => match tn {
                Some(tn) => tn,
                None => return self.fake_expr(),
            },
            None => {
                self.reveal(&mut typ);
                self.fail(CantMatch { location, typ });
                return self.fake_expr();
            }
        };
        let generics: HashMap<_, _> = generics
            .into_iter()
            .zip(&self.enums[name])
            .map(|(t, g)| (g.name, t))
            .collect();
        if DEBUG {
            for (n, t) in &generics {
                eprintln!("> {n} = {t}")
            }
        }
        let pattern_matches = match_expr
            .pattern_matches
            .into_iter()
            .map(|p| {
                if DEBUG {
                    eprintln!("> pattern {}", p.pattern.name);
                }
                let location = p.expr.location();
                let enum_name = self.labels[p.pattern.name].0;
                let mut ttyp = Type::Name(
                    enum_name,
                    self.enums[enum_name]
                        .iter()
                        .map(|g| Type::Generic(g.name))
                        .collect(),
                );
                self.type_context.sup.push(generics.clone());
                self.specify(&mut ttyp);
                self.type_context.pop_layer();
                typ = self.unify(location, take(&mut typ), ttyp);
                self.context.new_layer();
                let asg_mtyp = if let Some((lame, mmtyp)) = p.pattern.value {
                    let mut typ = self.labels[p.pattern.name].2.clone().unwrap();
                    self.type_context.sup.push(generics.clone());
                    self.specify(&mut typ);
                    self.type_context.pop_layer();
                    if let Some(typpppp) = mmtyp {
                        let typp = self.typ(&typpppp);
                        typ = self.unify(lame.location, typp, typ);
                    }
                    let res = self.asg_type(lame.location, &typ);
                    self.context.insert(lame.name, (typ, lame.location));
                    Some((lame.name, res))
                } else {
                    None
                };
                let (expr, expr_typ) = self.expr(p.expr).into();
                self.context.pop_layer();
                res_typ = self.unify(location, take(&mut res_typ), expr_typ);
                asg::PatternMatch {
                    label: self.labels[p.pattern.name].1,
                    mtyp: asg_mtyp,
                    expr,
                }
            })
            .collect();
        let expr_typ = self.asg_type(location, &typ);
        let typ = self.asg_type(match_expr.location, &res_typ);
        typed(
            asg::Match {
                expr,
                typ,
                pattern_matches,
                expr_typ,
            }
            .into(),
            res_typ,
        )
    }

    fn array(&mut self, array: Array<'a>) -> Typed<'a, asg::Tuple<'a>> {
        let mut typ = Type::Unknown;
        let exprs: Vec<_> = array
            .elems
            .into_iter()
            .map(|e| {
                let location = e.location();
                let (e, e_typ) = self.expr(e).into();
                let typ_ = std::mem::take(&mut typ);
                typ = self.unify(location, typ_, e_typ);
                (self.asg_type(location, &typ), e)
            })
            .collect();
        let len = exprs.len();
        let asg_type = self.asg_type(array.location, &typ);
        typed(
            asg::Tuple {
                exprs: vec![
                    (
                        asg::Type::Ptr(Box::new(asg_type)),
                        asg::Tuple { exprs }.into(),
                    ),
                    (
                        asg::Type::U64,
                        asg::Literal::Int(len as i64, asg::Type::U64).into(),
                    ),
                ],
            },
            Type::Name("slice", vec![typ]),
        )
    }

    fn cast(&mut self, cast: Cast<'a>) -> Typed<'a, asg::Cast<'a>> {
        let location = cast.expr.location();
        let (expr, from_typ) = self.expr(cast.expr).into();
        let to = self.typ(&cast.typ);
        let typ = self.cast_typ(cast.location, from_typ.clone(), to);
        let from = self.asg_type(location, &from_typ);
        let to = self.asg_type(cast.location, &typ);
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
            (_, Type::Prime(Prime::Unit)) => Type::Prime(Prime::Unit),
            (a, b) if a.is_ptr() && b.is_ptr() => b,
            (from, to) if from.is_number() && to.is_number() => to,
            (from, to) => {
                let from = from.clone();
                self.fail(NoCast { location, from, to });
                Type::Error
            }
        }
    }

    fn ret(&mut self, ret: Return<'a>) -> Typed<'a, asg::Return<'a>> {
        if DEBUG {
            eprintln!("> return")
        }
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
        let location = ref_expr.expr.location();
        let (expr, typ) = self.expr(ref_expr.expr).into();
        let expr_typ = self.asg_type(location, &typ);
        typed(asg::Ref { expr, expr_typ }, Type::Ref(Box::new(typ)))
    }

    fn negate(&mut self, negate: Negate<'a>) -> Typed<'a, asg::Negate<'a>> {
        let location = negate.expr.location();
        let (expr, typ) = self.expr(negate.expr).into();
        let expr_typ = self.asg_type(location, &typ);
        typed(asg::Negate { expr, expr_typ }, typ)
    }

    fn loop_expr(&mut self, loop_expr: Loop<'a>) -> Typed<'a, asg::Loop<'a>> {
        self.break_types.push(Type::Unreachable);
        let body = self.block(loop_expr.body.into()).sup;
        typed(asg::Loop { body }, self.break_types.pop().unwrap())
    }

    fn new_expr(&mut self, new: New<'a>, is_const: bool) -> Typed<'a, asg::Expr<'a>> {
        if DEBUG {
            eprintln!("> new {}", new.lame.name);
        }
        let typ = self.typ(&ast::Type::name(new.lame));
        let (name, generics) = match self.type_name(typ) {
            Some(name) => match name {
                Some(name) => name,
                None => return self.fake_expr(),
            },
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
            if DEBUG {
                eprintln!("> field {}", field.lame.name);
            }
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
                None => {
                    self.fail(NoField {
                        location: field.lame.location,
                        name: field.lame.name,
                        typ: Type::Name(name, generics),
                    });
                    return self.fake_expr();
                }
            };
            self.specify(&mut field_typ);
            let typ = self.unify(location, field_typ, typ);
            let asg_type = self.asg_type(field.lame.location, &typ);
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
        typed(asg::Tuple { exprs }.into(), Type::Name(name, generics))
    }

    fn assign(&mut self, assign: Assign<'a>) -> Typed<'a, asg::Assign<'a>> {
        let location = assign.expr.location();
        let (expr, expr_typ) = self.expr(assign.expr).into();
        let (to, to_typ) = self.expr(assign.to).into();
        let typ_ = self.unify(location, to_typ, expr_typ);
        let expr_type = self.asg_type(location, &typ_);
        typed(
            asg::Assign {
                expr,
                to,
                expr_type,
            },
            Prime::Unit.into(),
        )
    }

    fn fake_expr(&self) -> Typed<'a, asg::Expr<'a>> {
        typed(asg::Literal::Int(0, asg::Type::I32).into(), Type::Error)
    }

    fn field(&mut self, field_expr: ast::FieldExpr<'a>) -> Result<Typed<'a, asg::Field<'a>>, Fail> {
        let location = field_expr.expr.location();
        let (expr, mut typ) = self.expr(field_expr.expr).into();
        let is_ref = self.is_ref(&typ);
        let (name, generics) = match self.type_name(typ.clone()).ok_or_else(|| {
            self.reveal(&mut typ);
            self.fail(NoField {
                location: field_expr.lame.location,
                name: field_expr.lame.name,
                typ,
            })
        })? {
            Some(t) => t,
            None => return Err(Fail),
        };
        self.type_context.new_layer();
        self.specify_struct_generics(name, &generics);
        let Field { mut typ, id } = match self.structs[name].fields.get(field_expr.lame.name) {
            Some(field) => Ok(field),
            None => {
                let mut typ = Type::Name(name, generics);
                self.reveal(&mut typ);
                Err(self.fail(NoField {
                    location: field_expr.lame.location,
                    name: field_expr.lame.name,
                    typ,
                }))
            }
        }?
        .clone();
        self.specify(&mut typ);
        let mut generics = Vec::with_capacity(self.structs[name].generics.len());
        for g in self.structs[name].generics.clone() {
            let typ = self
                .type_context
                .sup
                .last_mut()
                .unwrap()
                .remove(g.name)
                .unwrap();
            generics.push(self.asg_type(location, &typ))
        }
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
                typ: self.asg_type(field_expr.lame.location, &typ),
                from_type,
            },
            typ,
        ))
    }

    fn is_ref(&self, typ: &Type<'a>) -> bool {
        match typ {
            Type::Ref(_) => true,
            Type::Var(id) => self.is_ref(&self.types[*id]),
            _ => false,
        }
    }

    fn specify_struct_generics(&mut self, name: &str, generics: &Vec<Type<'a>>) {
        for (index, generic) in self.structs[name].generics.iter().enumerate() {
            let typ = generics.get(index).cloned().unwrap_or_else(|| {
                let id = self.types.len();
                self.types.push(Type::Unknown);
                if DEBUG {
                    eprintln!("{id} = {}", self.types[id]);
                }
                Type::Var(id)
            });
            self.type_context.insert(generic.name, typ);
        }
    }

    fn type_name(&self, typ: Type<'a>) -> Option<Option<(&'a str, Vec<Type<'a>>)>> {
        if DEBUG {
            eprintln!("> type name of {typ}");
        }
        match typ {
            Type::Name(name, generics) => Some(Some((name, generics))),
            Type::Error => Some(None),
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
        let location = let_expr.expr.location();
        let name = let_expr.lame.name;
        let (expr, mut typ_) = self.expr(let_expr.expr).into();
        if let Some(typ) = let_expr.typ {
            let typ = self.typ(&typ);
            typ_ = self.unify(location, typ, typ_);
        }
        let typ = self.asg_type(location, &typ_);
        if DEBUG {
            eprintln!("{name} = {typ_}");
        }
        self.context
            .insert(let_expr.lame.name, (typ_, let_expr.lame.location));
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
        let asg_type = self.asg_type(location, &typ);
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
                    right: asg::Literal::SizeOf(asg_type.clone()).into(),
                    typ: asg::Type::U64,
                    args_typ: asg::Type::U64,
                }
                .into(),
                typ: asg::Type::U64,
                args_typ: asg::Type::U64,
            }
            .into(),
            typ: asg_type,
        };
        typed(res, typ)
    }

    fn deref(&mut self, deref: Deref<'a>) -> Typed<'a, asg::Deref<'a>> {
        let location = deref.expr.location();
        let (expr, typ) = self.expr(deref.expr).into();
        let typ = self.deref_type(typ, location);
        let asg_type = self.asg_type(location, &typ);
        let res = asg::Deref {
            expr,
            typ: asg_type,
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
    ) -> Result<Type<'a>, Box<CheckErrorKind<'a>>> {
        *expected = self
            .try_unify(location, *expected, found)
            .map_err(|mut err| {
                if let CheckErrorKind::WT(e) = err.as_mut() {
                    e.expected = Type::Ptr(Box::new(take(&mut e.expected)));
                    e.found = Type::Ptr(Box::new(take(&mut e.found)));
                };
                err
            })?;
        Ok(Type::Ptr(expected))
    }

    fn unify_refs(
        &mut self,
        location: Location<'a>,
        mut expected: Box<Type<'a>>,
        found: Type<'a>,
    ) -> Result<Type<'a>, Box<CheckErrorKind<'a>>> {
        *expected = self
            .try_unify(location, *expected, found)
            .map_err(|mut err| {
                if let CheckErrorKind::WT(e) = err.as_mut() {
                    e.expected = Type::Ref(Box::new(take(&mut e.expected)));
                    e.found = Type::Ref(Box::new(take(&mut e.found)));
                };
                err
            })?;
        Ok(Type::Ref(expected))
    }

    fn try_unify(
        &mut self,
        location: Location<'a>,
        expected: Type<'a>,
        found: Type<'a>,
    ) -> Result<Type<'a>, Box<CheckErrorKind<'a>>> {
        if DEBUG {
            eprintln!("> unify {expected} {found}");
        }
        match (expected, found) {
            (a, b) if a == b => Ok(a),
            (Type::Var(id), b) => {
                let a = self.types[id].clone();
                self.types[id] = self.try_unify(location, a, b)?;
                if DEBUG {
                    eprintln!("{id} = {}", self.types[id]);
                }
                Ok(Type::Var(id))
            }
            (a, Type::Var(id)) => {
                let b = self.types[id].clone();
                self.types[id] = self.try_unify(location, a, b)?;
                if DEBUG {
                    eprintln!("{id} = {}", self.types[id]);
                }
                Ok(Type::Var(id))
            }
            (_, Type::Error) => Ok(Type::Error),
            (Type::Error, _) => Ok(Type::Error),
            (Type::Unknown, b) => Ok(b),
            (a, Type::Unknown) => Ok(a),
            (a, Type::Unreachable) => Ok(a),
            (Type::Unreachable, b) => Ok(b),
            (Type::Ptr(expected), Type::Ptr(found)) => self.unify_ptrs(location, expected, *found),
            (Type::Ref(expected), Type::Ref(found)) => self.unify_refs(location, expected, *found),
            (Type::Fun(mut a), Type::Fun(mut b))
                if a.params.len() == b.params.len() && a.generics.len() == b.generics.len() =>
            {
                let len = a.params.len();
                for i in 0..len {
                    a.params[i] = match self.try_unify(
                        location,
                        take(&mut a.params[i]),
                        take(&mut b.params[i]),
                    ) {
                        Ok(typ) => typ,
                        Err(mut err) => {
                            if let CheckErrorKind::WT(err) = err.as_mut() {
                                for j in 0..len {
                                    if j != i {
                                        a.params[j] = Type::Error;
                                        b.params[j] = Type::Error;
                                    }
                                }
                                a.params[i] = take(&mut err.expected);
                                b.params[i] = take(&mut err.found);
                                err.expected = Type::Fun(a);
                                err.found = Type::Fun(b);
                            }
                            return Err(err);
                        }
                    };
                }
                a.ret_type =
                    match self.try_unify(location, take(&mut a.ret_type), take(&mut b.ret_type)) {
                        Ok(typ) => typ,
                        Err(mut err) => {
                            if let CheckErrorKind::WT(err) = err.as_mut() {
                                a.ret_type = take(&mut err.expected);
                                b.ret_type = take(&mut err.found);
                                err.expected = Type::Fun(a);
                                err.found = Type::Fun(b);
                            }
                            return Err(err);
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
                            if let CheckErrorKind::WT(err) = err.as_mut() {
                                for j in 0..len {
                                    if j != i {
                                        ga[j] = Type::Error;
                                        gb[j] = Type::Error;
                                    }
                                }
                                ga[i] = take(&mut err.expected);
                                gb[i] = take(&mut err.found);
                                err.expected = Type::Name(a, ga);
                                err.found = Type::Name(b, gb);
                            }
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
                Err(Box::new(
                    WrongType {
                        location,
                        expected,
                        found,
                    }
                    .into(),
                ))
            }
        }
    }

    fn reveal(&mut self, typ: &mut Type<'a>) {
        if DEBUG {
            eprintln!("> reveal {typ}")
        }
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
            Type::Var(id) => {
                *typ = self.types[*id].clone();
                self.reveal(typ);
            }
            Type::Unreachable => {}
            Type::Generic(_) => {}
            Type::Unknown => {}
            Type::Ref(typ) => self.reveal(typ),
        }
    }

    fn get_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
            Type::Var(id) => self.get_type(self.types[id].clone(), location),
            Type::Ptr(typ) => *typ,
            _ => {
                self.errors.push(
                    NoOp {
                        location,
                        typ,
                        op: "index",
                    }
                    .into(),
                );
                Type::Error
            }
        }
    }

    fn deref_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
            Type::Ref(typ) => *typ,
            Type::Ptr(typ) => *typ,
            _ => {
                self.fail(NoOp {
                    location,
                    typ,
                    op: "dereference",
                });
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
        let asg_type = self.asg_type(location, &typ);
        let res = asg::If {
            condition,
            then_expr,
            else_expr,
            typ: asg_type,
        };
        typed(res, typ)
    }

    fn call(&mut self, call: Call<'a>) -> Typed<'a, asg::Expr<'a>> {
        let name = call.lame.name;
        let location = call.lame.location;
        if let Some((enum_name, label, Some(mut typ))) = self.labels.get(name).cloned() {
            let generics = self.enums[enum_name]
                .clone()
                .into_iter()
                .map(|_| self.new_type_var(Type::Unknown))
                .collect();
            self.type_context.new_layer();
            for g in self.enums[enum_name].clone() {
                let typ = self.new_type_var(Type::Unknown);
                self.type_context.insert(g.name, typ);
            }
            self.specify(&mut typ);
            self.type_context.pop_layer();
            let location = call.args[0].location();
            let (expr, expr_typ) = self.expr(call.args[0].clone()).into();
            let typ = self.unify(location, typ, expr_typ);
            return typed(
                asg::Tuple {
                    exprs: vec![
                        (
                            asg::Type::U8,
                            asg::Literal::Int(label as i64, asg::Type::U8).into(),
                        ),
                        (self.asg_type(location, &typ), expr),
                    ],
                }
                .into(),
                Type::Name(enum_name, generics),
            );
        }
        let (expr, typ) = self.var(call.lame).into();
        let args: Vec<(Location, Typed<asg::Expr>)> = call
            .args
            .into_iter()
            .map(|e| (e.location(), self.expr(e)))
            .collect();
        let typ = match self.get_fun_type(typ, location) {
            Ok(typ) => typ,
            Err(_) => return self.fake_expr(),
        };
        if typ.params.len() != args.len() {
            self.fail(WrongCount {
                location,
                expected: typ.params.len(),
                found: args.len(),
            });
        }
        let args = args
            .into_iter()
            .zip(typ.params)
            .map(|((location, expr), expected)| {
                self.try_unify_or_ref(location, expr.sup, expected, expr.typ)
                    .unwrap_or_else(|err| {
                        self.fail(*err);
                        (asg::Type::Unit, asg::Expr::default())
                    })
            })
            .collect();
        let ret_type = self.asg_type(location, &typ.ret_type);
        typed(
            asg::Call {
                expr,
                args,
                ret_type,
            }
            .into(),
            typ.ret_type,
        )
    }

    fn try_unify_or_ref(
        &mut self,
        location: Location<'a>,
        expr: asg::Expr<'a>,
        expected: Type<'a>,
        found: Type<'a>,
    ) -> Result<(asg::Type<'a>, asg::Expr<'a>), Box<CheckErrorKind<'a>>> {
        match self.try_unify(location, expected.clone(), found.clone()) {
            Ok(typ) => {
                let asg_type = self.asg_type(location, &typ);
                Ok((asg_type, expr))
            }
            Err(err) => {
                let asg_type = self.asg_type(location, &found);
                if self
                    .try_unify(location, expected, Type::Ref(Box::new(found)))
                    .is_ok()
                {
                    let expr_typ = asg_type.clone();
                    Ok((
                        asg::Type::Ref(Box::new(asg_type)),
                        asg::Expr::Ref(Box::new(asg::Ref { expr, expr_typ })),
                    ))
                } else {
                    Err(err)
                }
            }
        }
    }

    fn specify_fun_type(&mut self, fun_type: &mut FunType<'a>) {
        for param in &mut fun_type.params {
            self.specify(param);
        }
        self.specify(&mut fun_type.ret_type);
    }

    fn specify(&mut self, typ: &mut Type<'a>) {
        if DEBUG {
            eprintln!("> specify {typ}");
        }
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
            Type::Unknown => {}
            Type::Ref(typ) => self.specify(typ),
        }
        if DEBUG {
            eprintln!("> specified to {typ}")
        }
    }

    fn get_fun_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Result<FunType<'a>, Fail> {
        match typ {
            Type::Error => Err(Fail),
            Type::Fun(typ) => Ok(*typ),
            _ => Err(self.fail(NoOp {
                location,
                typ,
                op: "invoke",
            })),
        }
    }

    fn var(&mut self, lame: Lame<'a>) -> Typed<'a, asg::Expr<'a>> {
        if DEBUG {
            eprintln!("> var {}", lame.name);
        }
        if let Some((typ, _)) = self.context.get(lame.name) {
            if DEBUG {
                eprintln!("> from context")
            }
            return typed(lame.name.into(), typ.clone());
        }
        if let Some((typ, _)) = self.global_funs.get(lame.name) {
            if DEBUG {
                eprintln!("> global function")
            }
            let mut typ = typ.clone();
            self.type_context.new_layer();
            for g in &typ.generics {
                let typ = self.new_type_var(Type::Unknown);
                if let Some(constraint) = &g.constraint {
                    let mut constraint = constraint.clone();
                    for typ in &mut constraint.generics {
                        self.specify(typ);
                    }
                    self.constraints
                        .push((lame.location, typ.clone(), constraint));
                }
                self.type_context.insert(g.name, typ);
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
                    (g.name, self.asg_type(lame.location, &typ))
                })
                .collect();
            let name = lame.name;
            return typed(asg::FunRef { name, generics }.into(), typ.into());
        }
        if let Some((name, label, _)) = self.labels.get(lame.name).cloned() {
            if DEBUG {
                eprintln!("> label of {name}")
            }
            let mut generics = Vec::with_capacity(self.enums[name].len());
            for _ in self.enums[name].clone() {
                generics.push(self.new_type_var(Type::Unknown));
            }
            return typed(
                asg::Tuple {
                    exprs: vec![(
                        asg::Type::U8,
                        asg::Literal::Int(label as i64, asg::Type::U8).into(),
                    )],
                }
                .into(),
                Type::Name(name, generics),
            );
        }
        if !self.corrupt.contains(lame.name) {
            self.corrupt.insert(lame.name);
            self.fail(NotDeclared {
                location: lame.location,
                kind: "item",
                name: lame.name,
            });
        }
        typed(lame.name.into(), Type::Error)
    }

    fn binary(&mut self, binary: Binary<'a>) -> Typed<'a, asg::Expr<'a>> {
        let left_location = binary.left.location();
        let (left, left_typ) = self.expr(binary.left).into();
        let right_location = binary.right.location();
        let (right, right_typ) = self.expr(binary.right).into();
        let op = self.bin_op(binary.op);
        let args_typ = self.asg_type(left_location, &left_typ);
        let typ = match binary.op {
            BinOp::Add
            | BinOp::Mod
            | BinOp::Div
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::Subtract
            | BinOp::Multiply
            | BinOp::And
            | BinOp::Or => {
                if matches!(left_typ, Type::Ptr(_)) {
                    self.unify(right_location, Prime::U64.into(), right_typ);
                    left_typ
                } else {
                    self.unify(right_location, left_typ, right_typ)
                }
            }
            BinOp::Equal | BinOp::NotEqual | BinOp::Less | BinOp::More => {
                self.unify(right_location, left_typ, right_typ);
                Prime::Bool.into()
            }
        };
        let asg_type = self.asg_type(binary.location, &typ);
        if let BinOp::And = binary.op {
            return typed(
                asg::If {
                    condition: left,
                    then_expr: right,
                    else_expr: asg::Literal::Int(0, asg::Type::Bool).into(),
                    typ: asg_type,
                }
                .into(),
                typ,
            );
        }
        if let BinOp::Or = binary.op {
            return typed(
                asg::If {
                    condition: left,
                    then_expr: asg::Literal::Int(1, asg::Type::Bool).into(),
                    else_expr: right,
                    typ: asg_type,
                }
                .into(),
                typ,
            );
        }
        let expr = match &typ {
            Type::Ptr(typ) => asg::Binary {
                left,
                op,
                right: asg::Binary {
                    left: right,
                    op: asg::BinOp::Multiply,
                    right: asg::Literal::SizeOf(self.asg_type(binary.location, typ)).into(),
                    typ: asg::Type::U64,
                    args_typ: asg::Type::U64,
                }
                .into(),
                typ: asg_type,
                args_typ: asg::Type::U64,
            },
            _ => asg::Binary {
                left,
                op,
                right,
                typ: asg_type,
                args_typ,
            },
        };
        typed(expr.into(), typ)
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::BitOr => asg::BinOp::Or,
            BinOp::Add => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
            BinOp::NotEqual => asg::BinOp::NotEqual,
            BinOp::Mod => asg::BinOp::Modulo,
            BinOp::Div => asg::BinOp::Divide,
            BinOp::BitAnd => asg::BinOp::And,
            BinOp::Subtract => asg::BinOp::Subtract,
            BinOp::Multiply => asg::BinOp::Multiply,
            BinOp::More => asg::BinOp::More,
            BinOp::And => asg::BinOp::And,
            BinOp::Or => asg::BinOp::Or,
        }
    }

    fn literal(
        &mut self,
        location: Location<'a>,
        literal: Literal<'a>,
    ) -> Typed<'a, asg::Literal<'a>> {
        match literal {
            Literal::Unit => typed(asg::Literal::Int(0, asg::Type::Unit), Prime::Unit.into()),
            Literal::Int(i) => {
                let typ = self.new_type_var(Type::Number);
                let asg_type = self.asg_type(location, &typ);
                typed(asg::Literal::Int(i, asg_type), typ)
            }
            Literal::Str(s) => typed(
                asg::Literal::Str(s),
                Type::Name("slice", vec![Prime::U8.into()]),
            ),
            Literal::Bool(b) => typed(
                asg::Literal::Int(b as i64, asg::Type::U8),
                Prime::Bool.into(),
            ),
            Literal::Size(typ) => {
                let typ = self.typ(&typ);
                typed(
                    asg::Literal::SizeOf(self.asg_type(location, &typ)),
                    Prime::U64.into(),
                )
            }
            Literal::Char(c) => typed(asg::Literal::Int(c as i64, asg::Type::U8), Prime::U8.into()),
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

    fn lame_type(&mut self, lame: Lame<'a>, generics: &[ast::Type<'a>]) -> Type<'a> {
        if DEBUG {
            eprintln!("> got {}", lame.name);
        }
        let mut generics: Vec<_> = generics.iter().map(|t| self.typ(t)).collect();
        if let Some(typ) = self.type_context.get(lame.name) {
            typ.clone()
        } else if let Some(typ) = self.type_aliases.remove(lame.name) {
            let typ = self.typ(&typ);
            self.type_context.sup[0].insert(lame.name, typ.clone());
            typ
        } else if let Some(struct_) = self.structs.get(lame.name) {
            if generics.is_empty() {
                let gs = struct_.generics.clone();
                generics = gs
                    .into_iter()
                    .map(|_| self.new_type_var(Type::Unknown))
                    .collect();
            }
            Type::Name(lame.name, generics)
        } else if let Some(gs) = self.enums.get(lame.name) {
            if generics.is_empty() {
                let gs = gs.clone();
                generics = gs
                    .into_iter()
                    .map(|_| self.new_type_var(Type::Unknown))
                    .collect()
            }
            Type::Name(lame.name, generics)
        } else if self.corrupt.contains(lame.name) {
            Type::Error
        } else if let Some(r#struct) = self.ast_structs.remove(lame.name) {
            self.r#struct(lame.name, r#struct);
            if generics.is_empty() {
                let gs = self.structs[lame.name].generics.clone();
                generics = gs
                    .into_iter()
                    .map(|_| self.new_type_var(Type::Unknown))
                    .collect();
            }
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
            let typ = self.typ(&field.typ);
            asg_fields.push(self.asg_type(field.typ.location(), &typ));
            fields.insert(field.lame.name, Field { id, typ });
        }
        self.type_context.pop_layer();
        let (generics, asg_generics) = r#struct
            .generics
            .into_iter()
            .map(|g| (self.generic(&g), g.name))
            .unzip();
        let r#struct = Struct { fields, generics };
        let asg_struct = asg::Struct {
            variants: vec![asg_fields],
            generics: asg_generics,
        };
        self.structs.insert(name, r#struct);
        self.asg_structs.insert(name, asg_struct);
    }

    fn enum_(&mut self, name: &'a str, variants: Vec<Variant<'a>>) {
        let mut asg_variants = Vec::with_capacity(variants.len());
        self.type_context.new_layer();
        for generic in &self.enums[name] {
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
        }
        for (i, variant) in variants.into_iter().enumerate() {
            let mut values = vec![asg::Type::U8];
            let mut mtyp = None;
            if let Some(typ) = variant.value {
                let location = typ.location();
                let typ = self.typ(&typ);
                values.push(self.asg_type(location, &typ));
                mtyp = Some(typ);
            }
            asg_variants.push(values);
            self.labels.insert(variant.lame.name, (name, i as u8, mtyp));
        }
        self.type_context.pop_layer();
        let asg_generics = self.enums[name].iter().map(|g| g.name).collect();
        let asg_struct = asg::Struct {
            variants: asg_variants,
            generics: asg_generics,
        };
        self.asg_structs.insert(name, asg_struct);
    }

    fn typ(&mut self, typ: &ast::Type<'a>) -> Type<'a> {
        match typ {
            ast::Type::Ptr(t, _) => Type::Ptr(Box::new(self.typ(t))),
            ast::Type::Name(lame, generics) => {
                let res = self.lame_type(*lame, generics);
                if DEBUG {
                    eprintln!("> made {res}");
                }
                res
            }
            ast::Type::Fun(fun_type) => self.fun_type(fun_type).into(),
            ast::Type::Prime(prime, _) => Type::Prime(*prime),
            ast::Type::Ref(t, _) => Type::Ref(Box::new(self.typ(t))),
        }
    }

    fn fun_type(&mut self, fun_type: &ast::FunType<'a>) -> FunType<'a> {
        self.type_context.new_layer();
        for generic in &fun_type.generics {
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
        }
        let res = FunType {
            generics: fun_type.generics.iter().map(|g| self.generic(g)).collect(),
            params: fun_type.params.iter().map(|t| self.typ(t)).collect(),
            ret_type: self.typ(&fun_type.ret),
        };
        self.type_context.pop_layer();
        res
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        if DEBUG {
            eprintln!("> fun {}", fun.header.lame.name);
        }
        self.context.new_layer();
        self.type_context.new_layer();
        self.g_constraints.new_layer();
        for generic in fun.header.typ.generics {
            let generic = self.generic(&generic);
            self.type_context
                .insert(generic.name, Type::Generic(generic.name));
            if let Some(constraint) = generic.constraint {
                self.g_constraints.insert(generic.name, constraint);
            }
        }
        let params = fun
            .header
            .params
            .iter()
            .zip(fun.header.typ.params)
            .map(|(param, typ)| {
                let typ_ = self.typ(&typ);
                let typ = self.asg_type(typ.location(), &typ_);
                self.context.insert(param.name, (typ_, param.location));
                (param.name, typ)
            })
            .collect();
        let ret_typ = self.typ(&fun.header.typ.ret);
        let ret_type = self.asg_type(fun.header.typ.ret.location(), &ret_typ);
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
        let body = match self.try_unify_or_ref(location, body, self.ret_type.clone(), typ) {
            Ok((_, body)) => body,
            Err(err) => {
                self.fail(*err);
                if let Some(location) = last_semi_location
                    && is_unit
                {
                    self.errors.last_mut().unwrap().help =
                        Some(SemicolonEndBlock { location }.into());
                }
                asg::Expr::default()
            }
        };
        while let Some((location, typ, constraint)) = self.constraints.pop() {
            self.constrain(location, typ, constraint);
        }
        self.context.pop_layer();
        self.type_context.pop_layer();
        self.g_constraints.pop_layer();
        if DEBUG {
            eprintln!("> generated {:?}", body);
        }
        asg::Fun {
            params,
            body,
            ret_type,
        }
    }

    fn asg_type(&mut self, location: Location<'a>, typ: &Type<'a>) -> asg::Type<'a> {
        if DEBUG {
            eprintln!("> asg type {typ}")
        }
        match self.try_hot_type(typ) {
            Some(typ) => typ,
            None => self.new_cold_type(location, typ.clone()),
        }
    }

    fn try_hot_type(&self, typ: &Type<'a>) -> Option<asg::Type<'a>> {
        if DEBUG {
            eprintln!("> try hot {typ}");
        }
        match typ {
            Type::Ptr(typ) => Some(asg::Type::Ptr(Box::new(self.try_hot_type(typ)?))),
            Type::Name(name, generics) => Some(asg::Type::Name(
                name,
                generics
                    .iter()
                    .map(|t| self.try_hot_type(t))
                    .collect::<Option<_>>()?,
            )),
            Type::Fun(typ) => {
                let mut typs = typ
                    .params
                    .iter()
                    .map(|typ| self.try_hot_type(typ))
                    .collect::<Option<Vec<_>>>()?;
                typs.push(self.try_hot_type(&typ.ret_type)?);
                Some(asg::Type::FunPtr(typs))
            }
            Type::Prime(prime) => Some(self.asg_prime(prime)),
            Type::Error => Some(asg::Type::I32),
            Type::Number => None,
            Type::Var(id) => self.try_hot_type(&self.types[*id]),
            Type::Unreachable => Some(asg::Type::I32),
            Type::Generic(g) => Some(asg::Type::Generic(g)),
            Type::Unknown => None,
            Type::Ref(typ) => Some(asg::Type::Ref(Box::new(self.try_hot_type(typ)?))),
        }
    }

    fn new_cold_type(&mut self, location: Location<'a>, typ: Type<'a>) -> asg::Type<'a> {
        if DEBUG {
            eprintln!("> made cold {typ}");
        }
        let id = self.cold_types.len();
        self.cold_types.push((location, typ));
        asg::Type::Cold(id)
    }

    fn asg_prime(&self, prime: &Prime) -> asg::Type<'a> {
        match prime {
            Prime::Unit => asg::Type::I32,
            Prime::Bool => asg::Type::Bool,
            Prime::I32 => asg::Type::I32,
            Prime::U8 => asg::Type::U8,
            Prime::U64 => asg::Type::U64,
            Prime::I64 => asg::Type::I64,
            Prime::F32 => asg::Type::F32,
            Prime::F64 => asg::Type::F64,
        }
    }
}
