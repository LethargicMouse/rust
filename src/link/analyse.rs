mod error;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use error::Error;

use crate::{
    Location, die,
    display::Sep,
    link::{
        Asg, Context,
        analyse::error::{
            CheckError, Fail, NoCall, NoField, NoIndex, NotDeclared, ShouldKnowType, WrongType,
        },
        asg::{self, Tuple},
        ast::{self, *},
    },
};

#[derive(Clone, PartialEq)]
struct FunType<'a> {
    params: Vec<Type<'a>>,
    ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({}) {}", Sep(", ", &self.params), self.ret_type)
    }
}

#[derive(Clone, PartialEq, Default)]
enum Type<'a> {
    Ptr(Box<Type<'a>>),
    Name(&'a str),
    Fun(Box<FunType<'a>>),
    Prime(Prime),
    #[default]
    Error,
    Number,
    Var(usize),
    Unreachable,
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
            Type::Name(n) => write!(f, "{n}"),
            Type::Error => write!(f, "<error>"),
            Type::Fun(fun_type) => write!(f, "{fun_type}"),
            Type::Prime(prime) => write!(f, "{prime}"),
            Type::Number => write!(f, "<number>"),
            Type::Var(id) => write!(f, "<#{id}>"),
            Type::Unreachable => write!(f, "<unreachable>"),
        }
    }
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

impl<'a> Type<'a> {
    fn name(&self) -> Option<&'a str> {
        match self {
            Type::Name(n) => Some(n),
            _ => None,
        }
    }

    fn is_number(&self) -> bool {
        match self {
            Type::Ptr(_) => false,
            Type::Name(_) => false,
            Type::Fun(_) => false,
            Type::Prime(prime) => prime.is_number(),
            Type::Error => true,
            Type::Number => true,
            Type::Var(_) => false,
            Type::Unreachable => true,
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

struct Field<'a> {
    pub offset: u32,
    pub typ: Type<'a>,
}

struct Struct<'a> {
    fields: HashMap<&'a str, Field<'a>>,
    size: u32,
    align: u32,
}

pub struct Info<'a> {
    pub types: Vec<asg::Type<'a>>,
}

struct Analyse<'a> {
    ast_structs: HashMap<&'a str, ast::Struct<'a>>,
    structs: HashMap<&'a str, Struct<'a>>,
    errors: Vec<CheckError<'a>>,
    context: Context<'a, Type<'a>>,
    corrupt: HashSet<&'a str>,
    cold_types: Vec<(Type<'a>, Location<'a>)>,
    types: Vec<Type<'a>>,
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
        }
    }

    pub fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        self.ast_structs = ast.structs;
        self.typ(ast::Type::Name(Lame {
            name: "str",
            location: ast.begin,
        }));
        for extrn in ast.externs {
            let typ = self.typ(extrn.typ);
            self.context.insert(extrn.name, typ);
        }
        for fun in &ast.funs {
            let typ = self.typ(fun.header.typ.clone().into());
            self.context.insert(fun.header.name, typ);
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|fun| (fun.header.name, self.fun(fun)))
            .collect();
        let info = self.info();
        if !self.errors.is_empty() {
            die(Error(self.errors))
        }
        Asg { funs, info }
    }

    fn info(&mut self) -> Info<'a> {
        let cold_types = std::mem::take(&mut self.cold_types);
        let types: Vec<_> = cold_types
            .into_iter()
            .map(|(typ, location)| self.hot_type(&typ, location))
            .collect();
        Info { types }
    }

    fn hot_type(&mut self, typ: &Type<'a>, location: Location<'a>) -> asg::Type<'a> {
        self.try_hot_type(typ).unwrap_or_else(|| {
            self.fail(ShouldKnowType { location });
            asg::Type::I32
        })
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
            Expr::New(new) => self.new_expr(new),
            Expr::Loop(loop_expr) => self.loop_expr(*loop_expr).map_into(),
            Expr::Ref(ref_expr) => self.ref_expr(*ref_expr).map_into(),
            Expr::Return(ret) => self.ret(*ret).map_into(),
        }
    }

    fn ret(&mut self, ret: Return<'a>) -> Typed<'a, asg::Return<'a>> {
        let expr = self.expr(ret.expr).sup;
        typed(asg::Return { expr }, Type::Unreachable)
    }

    fn ref_expr(&mut self, ref_expr: Ref<'a>) -> Typed<'a, asg::Ref<'a>> {
        let (expr, typ) = self.expr(ref_expr.expr).into();
        typed(asg::Ref { expr }, Type::Ptr(Box::new(typ)))
    }

    fn loop_expr(&mut self, loop_expr: Loop<'a>) -> Typed<'a, asg::Loop<'a>> {
        let (body, typ) = self.block(loop_expr.body).into();
        typed(asg::Loop { body }, typ)
    }

    fn new_expr(&mut self, new: New<'a>) -> Typed<'a, asg::Expr<'a>> {
        let typ = self.typ(ast::Type::Name(new.lame));
        let exprs = Vec::new();
        let align = 1;
        typed(asg::Expr::Tuple(Tuple { exprs, align }), typ)
    }

    fn assign(&mut self, assign: Assign<'a>) -> Typed<'a, asg::Assign<'a>> {
        let (expr, typ) = self.expr(assign.expr).into();
        typed(
            asg::Assign {
                expr,
                to: self.expr(assign.to).sup,
                expr_type: self.asg_type(&typ, assign.location),
            },
            Prime::Unit.into(),
        )
    }

    fn fake_expr(&self) -> Typed<'a, asg::Expr<'a>> {
        typed(asg::Literal::Int(0).into(), Type::Error)
    }

    fn field(&mut self, field_expr: ast::FieldExpr<'a>) -> Result<Typed<'a, asg::Field<'a>>, Fail> {
        let (from, typ) = self.expr(field_expr.from).into();
        if matches!(typ, Type::Error) {
            return Err(Fail);
        }
        let name = typ.name().ok_or_else(|| {
            self.fail(NoField {
                location: field_expr.name_location,
                name: field_expr.name,
                typ: typ.clone(),
            })
        })?;
        let field = match self.structs[name].fields.get(field_expr.name) {
            Some(field) => Ok(field),
            None => Err(self.fail(NoField {
                location: field_expr.name_location,
                name: field_expr.name,
                typ,
            })),
        }?;
        let offset = field.offset;
        let typ_ = field.typ.clone();
        let typ = self.asg_type(&typ_, field_expr.name_location);
        Ok(typed(asg::Field { from, offset, typ }, typ_))
    }

    fn fail(&mut self, error: impl Into<CheckError<'a>>) -> Fail {
        self.errors.push(error.into());
        Fail
    }

    fn let_expr(&mut self, let_expr: Let<'a>) -> Typed<'a, asg::Let<'a>> {
        let name = let_expr.name;
        let location = let_expr.expr.location();
        let (expr, typ_) = self.expr(let_expr.expr).into();
        let typ = self.asg_type(&typ_, location);
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
                    right: asg::Literal::Int(8).into(),
                }
                .into(),
            }
            .into(),
            typ: self.asg_type(&typ, get.location),
        };
        typed(res, typ)
    }

    fn unify(&mut self, location: Location<'a>, expected: Type<'a>, found: Type<'a>) -> Type<'a> {
        match (expected, found) {
            (a, b) if a == b => a,
            (a, Type::Error) => a,
            (Type::Error, b) => b,
            (a, Type::Unreachable) => a,
            (a, Type::Var(id)) => {
                let b = std::mem::take(&mut self.types[id]);
                self.types[id] = self.unify(location, a, b);
                Type::Var(id)
            }
            (Type::Ptr(mut a), Type::Ptr(b)) => {
                *a = self.unify(location, *a, *b);
                Type::Ptr(a)
            }
            (a, Type::Number) if a.is_number() => a,
            (expected, found) => {
                self.errors.push(
                    WrongType {
                        location,
                        expected,
                        found,
                    }
                    .into(),
                );
                Type::Error
            }
        }
    }

    fn get_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
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
        let name = call.var.name;
        let location = call.var.location;
        let typ = self.var(call.var).typ;
        let args_locations: Vec<_> = call.args.iter().map(|a| a.location()).collect();
        let (args, args_types): (_, Vec<_>) =
            call.args.into_iter().map(|e| self.expr(e).into()).unzip();
        let typ = self.get_fun_type(typ, location)?;
        for ((found, expected), location) in
            args_types.into_iter().zip(typ.params).zip(args_locations)
        {
            self.unify(location, expected, found);
        }
        Ok(typed(asg::Call { name, args }, typ.ret_type))
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
                self.errors.push(
                    NotDeclared {
                        location: var.location,
                        kind: "item",
                        name: var.name,
                    }
                    .into(),
                );
                typed(var.name, Type::Error)
            }
        }
    }

    fn binary(&mut self, binary: Binary<'a>) -> Typed<'a, asg::Binary<'a>> {
        let (left, typ) = self.expr(binary.left).into();
        let right = self.expr(binary.right).sup;
        let typ = match binary.op {
            BinOp::Plus => typ,
            BinOp::Equal | BinOp::NotEqual | BinOp::Less => Prime::Bool.into(),
        };
        let op = self.bin_op(binary.op);
        typed(asg::Binary { left, op, right }, typ)
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
            BinOp::NotEqual => asg::BinOp::NotEqual,
        }
    }

    fn literal(&mut self, literal: Literal<'a>) -> Typed<'a, asg::Literal<'a>> {
        match literal {
            Literal::Unit => typed(asg::Literal::Int(0), Prime::Unit.into()),
            Literal::Int(i) => typed(asg::Literal::Int(i), self.new_type_var(Type::Number)),
            Literal::Str(s) => typed(asg::Literal::Str(s), Type::Name("str")),
        }
    }

    fn new_type_var(&mut self, typ: Type<'a>) -> Type<'a> {
        let id = self.types.len();
        self.types.push(typ);
        Type::Var(id)
    }

    fn struct_type(&mut self, Lame { name, location }: Lame<'a>) -> Type<'a> {
        if self.structs.contains_key(name) {
            Type::Name(name)
        } else if self.corrupt.contains(name) {
            Type::Error
        } else {
            match self.ast_structs.remove(name) {
                Some(r#struct) => {
                    self.r#struct(name, r#struct);
                    Type::Name(name)
                }
                None => {
                    self.corrupt.insert(name);
                    self.errors.push(
                        NotDeclared {
                            location,
                            kind: "struct",
                            name,
                        }
                        .into(),
                    );
                    Type::Error
                }
            }
        }
    }

    fn r#struct(&mut self, name: &'a str, r#struct: ast::Struct<'a>) {
        let mut fields = HashMap::new();
        let mut offset = 0;
        let mut align = 1;
        for field in r#struct.fields {
            let typ = self.typ(field.typ);
            let size = self.try_hot_size(&typ).unwrap();
            align = align.max(self.try_hot_align(&typ).unwrap());
            fields.insert(field.name, Field { offset, typ });
            offset += size;
        }
        let r#struct = Struct {
            fields,
            size: offset,
            align,
        };
        self.structs.insert(name, r#struct);
    }

    fn try_hot_size(&self, typ: &Type<'a>) -> Option<u32> {
        match typ {
            Type::Ptr(_) => Some(8),
            Type::Name(name) => Some(self.structs[name].size),
            Type::Fun(_) => Some(8),
            Type::Prime(prime) => Some(prime.size()),
            Type::Error => Some(0),
            Type::Number => None,
            &Type::Var(id) => self.try_hot_size(&self.types[id]),
            Type::Unreachable => Some(0),
        }
    }

    fn try_hot_align(&mut self, typ: &Type<'a>) -> Option<u32> {
        match typ {
            Type::Name(name) => Some(self.structs[name].align),
            _ => self.try_hot_size(typ),
        }
    }

    fn typ(&mut self, typ: ast::Type<'a>) -> Type<'a> {
        match typ {
            ast::Type::Ptr(t, _) => Type::Ptr(Box::new(self.typ(*t))),
            ast::Type::Name(var) => self.struct_type(var),
            ast::Type::Fun(fun_type) => self.fun_type(*fun_type).into(),
            ast::Type::Prime(prime, _) => Type::Prime(prime),
        }
    }

    fn fun_type(&mut self, fun_type: ast::FunType<'a>) -> FunType<'a> {
        let params = fun_type.params.into_iter().map(|t| self.typ(t)).collect();
        let ret_type = self.typ(fun_type.ret);
        FunType { params, ret_type }
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        self.context.new_layer();
        let params = fun
            .header
            .params
            .iter()
            .zip(fun.header.typ.params)
            .map(|(param, typ)| {
                let location = typ.location();
                let typ_ = self.typ(typ);
                let typ = self.asg_type(&typ_, location);
                self.context.insert(param, typ_);
                (*param, typ)
            })
            .collect();
        let location = fun.header.typ.ret.location();
        let ret_typ = self.typ(fun.header.typ.ret);
        let ret_type = self.asg_type(&ret_typ, location);
        let location = fun.body.location();
        let (body, typ) = self.expr(fun.body).into();
        self.unify(location, ret_typ, typ);
        asg::Fun {
            params,
            body,
            ret_type,
        }
    }

    fn asg_type(&mut self, typ: &Type<'a>, location: Location<'a>) -> asg::Type<'a> {
        match self.try_hot_type(typ) {
            Some(typ) => typ,
            None => self.new_cold_type(typ.clone(), location),
        }
    }

    fn try_hot_type(&self, typ: &Type<'a>) -> Option<asg::Type<'a>> {
        match typ {
            Type::Ptr(_) => Some(asg::Type::U64),
            Type::Name(name) => Some(asg::Type::Name {
                name,
                size: self.structs[name].size,
                align: self.structs[name].align,
            }),
            Type::Fun(_) => Some(asg::Type::U64),
            Type::Prime(prime) => Some(self.asg_prime(prime)),
            Type::Error => Some(asg::Type::I32),
            Type::Number => None,
            &Type::Var(id) => self.try_hot_type(&self.types[id]),
            Type::Unreachable => Some(asg::Type::I32),
        }
    }

    fn new_cold_type(&mut self, typ: Type<'a>, location: Location<'a>) -> asg::Type<'a> {
        let id = self.cold_types.len();
        self.cold_types.push((typ, location));
        asg::Type::Cold(id)
    }

    fn asg_prime(&self, prime: &Prime) -> asg::Type<'a> {
        match prime {
            Prime::Unit => asg::Type::I32,
            Prime::Bool => asg::Type::U8,
            Prime::I32 => asg::Type::I32,
            Prime::U8 => asg::Type::U8,
            Prime::U64 => asg::Type::U64,
        }
    }
}
