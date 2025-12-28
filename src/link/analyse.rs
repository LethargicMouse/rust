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
        analyse::error::{CheckError, Fail, NoCall, NoField, NoIndex, NotDeclared},
        asg,
        ast::{self, *},
    },
};

#[derive(Clone)]
struct FunType<'a> {
    params: Vec<Type<'a>>,
    ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({}) {}", Sep(", ", &self.params), self.ret_type)
    }
}

#[derive(Clone)]
enum Type<'a> {
    Ptr(Box<Type<'a>>),
    Name(&'a str),
    Fun(Box<FunType<'a>>),
    Prime(Prime),
    Error,
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
    pub offset: usize,
    pub typ: Type<'a>,
}

struct Struct<'a> {
    fields: HashMap<&'a str, Field<'a>>,
}

struct State<'a> {
    errors: Vec<CheckError<'a>>,
    context: Context<'a, Type<'a>>,
    corrupt: HashSet<&'a str>,
}

impl<'a> State<'a> {
    fn new() -> Self {
        Self {
            context: Context::new(),
            errors: Vec::new(),
            corrupt: HashSet::new(),
        }
    }
}

struct Analyse<'a> {
    structs: HashMap<&'a str, Struct<'a>>,
    sup: State<'a>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        Self {
            sup: State::new(),
            structs: HashMap::new(),
        }
    }

    pub fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        for extrn in ast.externs {
            let typ = self.typ(extrn.typ);
            self.sup.context.insert(extrn.name, typ);
        }
        for fun in &ast.funs {
            let typ = self.typ(fun.header.typ.clone().into());
            self.sup.context.insert(fun.header.name, typ);
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|fun| (fun.header.name, self.fun(fun)))
            .collect();
        if !self.sup.errors.is_empty() {
            die(Error(self.sup.errors))
        }
        Asg { funs }
    }

    fn expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Call(call) => self.call(call).map_into(),
            Expr::Binary(binary) => self.binary(*binary).map_into(),
            Expr::Literal(literal, _) => self.literal(literal).map_into(),
            Expr::Var(name) => self.var(name).map_into(),
            Expr::If(if_expr) => self.if_expr(*if_expr).map_into(),
            Expr::Get(get) => self.get(*get),
            Expr::Block(block) => self.block(*block).map_into(),
            Expr::Let(let_expr) => self.let_expr(*let_expr).map_into(),
            Expr::Field(field) => match self.field(*field) {
                Ok(f) => f.map_into(),
                Err(_) => self.fake_expr(),
            },
        }
    }

    fn fake_expr(&self) -> Typed<'a, asg::Expr<'a>> {
        typed(asg::Literal::Int(0).into(), Type::Error)
    }

    fn field(&mut self, field: ast::Field<'a>) -> Result<Typed<'a, asg::Field<'a>>, Fail> {
        let (from, typ) = self.expr(field.from).into();
        if matches!(typ, Type::Error) {
            return Err(Fail);
        }
        let name = typ.name().ok_or_else(|| {
            self.fail(NoField {
                location: field.name_location,
                name: field.name,
                typ: typ.clone(),
            })
        })?;
        let r#struct = self.structs.get(name).unwrap();
        let field = r#struct.fields.get(field.name).ok_or_else(|| {
            self.sup.errors.push(
                NoField {
                    location: field.name_location,
                    name: field.name,
                    typ,
                }
                .into(),
            );
            Fail
        })?;
        let offset = field.offset;
        let typ = field.typ.clone();
        Ok(typed(asg::Field { from, offset }, typ))
    }

    fn fail(&mut self, error: impl Into<CheckError<'a>>) -> Fail {
        self.sup.errors.push(error.into());
        Fail
    }

    fn let_expr(&mut self, let_expr: Let<'a>) -> Typed<'a, asg::Let<'a>> {
        let name = let_expr.name;
        let (expr, typ) = self.expr(let_expr.expr).into();
        self.sup.context.insert(name, typ);
        typed(asg::Let { name, expr }, Prime::Unit.into())
    }

    fn block(&mut self, block: Block<'a>) -> Typed<'a, asg::Block<'a>> {
        let stmts: Vec<_> = block.stmts.into_iter().map(|e| self.expr(e).sup).collect();
        let (ret, typ) = self.expr(block.ret).into();
        typed(asg::Block { stmts, ret }, typ)
    }

    fn get(&mut self, get: Get<'a>) -> Typed<'a, asg::Expr<'a>> {
        let location = get.from.location();
        let from = self.expr(get.from);
        let typ = self.index_type(from.typ, location);
        let index = self.expr(get.index);
        let res = asg::Expr::Deref(Box::new(
            asg::Binary {
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
        ));
        typed(res, typ)
    }

    fn index_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
            Type::Ptr(typ) => *typ,
            _ => {
                self.sup.errors.push(NoIndex { location, typ }.into());
                Type::Error
            }
        }
    }

    fn if_expr(&mut self, if_expr: If<'a>) -> Typed<'a, asg::If<'a>> {
        let condition = self.expr(if_expr.condition).sup;
        let (then_expr, typ) = self.expr(if_expr.then_expr).into();
        let else_expr = self.expr(if_expr.else_expr).sup;
        let res = asg::If {
            condition,
            then_expr,
            else_expr,
        };
        typed(res, typ)
    }

    fn call(&mut self, call: Call<'a>) -> Typed<'a, asg::Call<'a>> {
        let name = call.var.name;
        let location = call.var.location;
        let fun_typ = self.var(call.var).typ;
        let typ = self.ret_type(fun_typ, location);
        let args = call.args.into_iter().map(|e| self.expr(e).sup).collect();
        typed(asg::Call { name, args }, typ)
    }

    fn ret_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Error => Type::Error,
            Type::Fun(typ) => typ.ret_type,
            _ => {
                self.sup.errors.push(NoCall { location, typ }.into());
                Type::Error
            }
        }
    }

    fn var(&mut self, var: Lame<'a>) -> Typed<'a, &'a str> {
        match self.sup.context.get(var.name) {
            Some(typ) => typed(var.name, typ.clone()),
            None => {
                self.sup.errors.push(
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
            BinOp::Equal | BinOp::Less => Prime::Bool.into(),
        };
        let op = self.bin_op(binary.op);
        typed(asg::Binary { left, op, right }, typ)
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
        }
    }

    fn literal(&self, literal: Literal<'a>) -> Typed<'a, asg::Literal<'a>> {
        match literal {
            Literal::Unit => typed(asg::Literal::Int(0), Prime::Unit.into()),
            Literal::Int(i) => typed(asg::Literal::Int(i), Prime::I32.into()),
            Literal::RawStr(s) => {
                typed(asg::Literal::Str(s), Type::Ptr(Box::new(Prime::U8.into())))
            }
        }
    }

    fn r#struct(&mut self, Lame { name, location }: Lame<'a>) -> Type<'a> {
        if self.structs.contains_key(name) {
            Type::Name(name)
        } else if self.sup.corrupt.contains(name) {
            Type::Error
        } else {
            self.sup.corrupt.insert(name);
            self.sup.errors.push(
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

    fn typ(&mut self, typ: ast::Type<'a>) -> Type<'a> {
        match typ {
            ast::Type::Ptr(t) => Type::Ptr(Box::new(self.typ(*t))),
            ast::Type::Name(var) => self.r#struct(var),
            ast::Type::Fun(fun_type) => self.fun_type(*fun_type).into(),
            ast::Type::Prime(prime) => Type::Prime(prime),
        }
    }

    fn fun_type(&mut self, fun_type: ast::FunType<'a>) -> FunType<'a> {
        let params = fun_type.params.into_iter().map(|t| self.typ(t)).collect();
        let ret_type = self.typ(fun_type.ret_type);
        FunType { params, ret_type }
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        self.sup.context.new_layer();
        for (param, typ) in fun.header.params.iter().zip(fun.header.typ.params) {
            let typ = self.typ(typ);
            self.sup.context.insert(param, typ);
        }
        let params = fun.header.params;
        let body = self.expr(fun.body).sup;
        asg::Fun { params, body }
    }
}
