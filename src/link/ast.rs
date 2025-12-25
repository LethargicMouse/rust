use std::fmt::Display;

use crate::{Location, display::Sep};

pub struct Ast<'a> {
    pub funs: Vec<Fun<'a>>,
    pub externs: Vec<Extern<'a>>,
}

pub struct Extern<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub struct Struct<'a> {
    pub name: &'a str,
}

pub enum Item<'a> {
    Fun(Fun<'a>),
    Extern(Extern<'a>),
    Struct(Struct<'a>),
}

impl<'a> From<Struct<'a>> for Item<'a> {
    fn from(v: Struct<'a>) -> Self {
        Self::Struct(v)
    }
}

impl<'a> From<Extern<'a>> for Item<'a> {
    fn from(v: Extern<'a>) -> Self {
        Self::Extern(v)
    }
}

impl<'a> From<Fun<'a>> for Item<'a> {
    fn from(v: Fun<'a>) -> Self {
        Self::Fun(v)
    }
}

pub struct Fun<'a> {
    pub header: Header<'a>,
    pub body: Expr<'a>,
}

pub struct Header<'a> {
    pub name: &'a str,
    pub params: Vec<&'a str>,
    pub type_params_locations: Vec<Location<'a>>,
    pub typ: FunType<'a>,
}

#[derive(Clone)]
pub struct FunType<'a> {
    pub params: Vec<Type<'a>>,
    pub ret_type: Type<'a>,
}

impl Display for FunType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({}) {}", Sep(", ", &self.params), self.ret_type)
    }
}

pub enum Literal<'a> {
    Unit,
    Int(i64),
    RawStr(&'a str),
}

pub struct If<'a> {
    pub location: Location<'a>,
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
}

pub struct Block<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub struct Let<'a> {
    pub location: Location<'a>,
    pub name: &'a str,
    pub expr: Expr<'a>,
}

pub struct Field<'a> {
    pub from: Expr<'a>,
    pub name: &'a str,
    pub name_location: Location<'a>,
}

pub struct Var<'a> {
    pub name: &'a str,
    pub location: Location<'a>,
}

pub enum Expr<'a> {
    Field(Box<Field<'a>>),
    Let(Box<Let<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>, Location<'a>),
    If(Box<If<'a>>),
    Var(Var<'a>),
    Get(Box<Get<'a>>),
    Block(Box<Block<'a>>),
}

impl<'a> From<Field<'a>> for Expr<'a> {
    fn from(v: Field<'a>) -> Self {
        Self::Field(Box::new(v))
    }
}

impl<'a> From<Let<'a>> for Expr<'a> {
    fn from(v: Let<'a>) -> Self {
        Self::Let(Box::new(v))
    }
}

impl<'a> From<If<'a>> for Expr<'a> {
    fn from(value: If<'a>) -> Self {
        Expr::If(Box::new(value))
    }
}

impl<'a> From<Block<'a>> for Expr<'a> {
    fn from(value: Block<'a>) -> Self {
        Expr::Block(Box::new(value))
    }
}

impl<'a> Expr<'a> {
    pub fn location(&self) -> Location<'a> {
        match self {
            Expr::Field(field) => field.name_location,
            Expr::Let(let_expr) => let_expr.location,
            Expr::Call(call) => call.var.location,
            Expr::Binary(binary) => binary.op_location,
            Expr::Literal(_, location) => *location,
            Expr::If(if_expr) => if_expr.location,
            Expr::Var(var) => var.location,
            Expr::Get(get) => get.location,
            Expr::Block(block) => block.ret.location(),
        }
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            Expr::Call(_) => true,
            Expr::Binary(_) => true,
            Expr::Literal(_, _) => true,
            Expr::If(if_expr) => {
                if matches!(if_expr.else_expr, Expr::Literal(Literal::Unit, _)) {
                    if_expr.then_expr.needs_semicolon()
                } else {
                    if_expr.else_expr.needs_semicolon()
                }
            }
            Expr::Var(_) => true,
            Expr::Get(_) => true,
            Expr::Block(_) => false,
            Expr::Let(_) => true,
            Expr::Field(_) => true,
        }
    }
}

impl<'a> From<Var<'a>> for Expr<'a> {
    fn from(v: Var<'a>) -> Self {
        Self::Var(v)
    }
}

impl<'a> From<Get<'a>> for Expr<'a> {
    fn from(v: Get<'a>) -> Self {
        Self::Get(Box::new(v))
    }
}

impl<'a> From<Binary<'a>> for Expr<'a> {
    fn from(v: Binary<'a>) -> Self {
        Self::Binary(Box::new(v))
    }
}

pub struct Call<'a> {
    pub var: Var<'a>,
    pub args: Vec<Expr<'a>>,
}

pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
    pub op_location: Location<'a>,
    pub right: Expr<'a>,
}

pub enum BinOp {
    Plus,
    Equal,
    Less,
}

pub enum Postfix<'a> {
    Get(Expr<'a>),
    Call(Call<'a>),
    Field(&'a str, Location<'a>),
}

impl<'a> From<Call<'a>> for Postfix<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

pub struct Get<'a> {
    pub from: Expr<'a>,
    pub index: Expr<'a>,
    pub location: Location<'a>,
}

#[derive(Clone)]
pub enum Type<'a> {
    Ptr(Box<Type<'a>>),
    Name(&'a str),
    Fun(Box<FunType<'a>>),
    Unit,
    Error,
}

impl Default for Type<'_> {
    fn default() -> Self {
        Self::Error
    }
}

impl<'a> From<&'a str> for Type<'a> {
    fn from(v: &'a str) -> Self {
        Self::Name(v)
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name(n) => write!(f, "{n}"),
            Type::Fun(fun_type) => write!(f, "{fun_type}"),
            Type::Unit => write!(f, "()"),
            Type::Error => write!(f, "<error>"),
            Type::Ptr(t) => write!(f, "*{t}"),
        }
    }
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

impl<'a> Type<'a> {
    pub fn name(&self) -> Option<&'a str> {
        match self {
            Type::Name(n) => Some(n),
            _ => None,
        }
    }
}
