use std::{collections::HashMap, fmt::Display};

use crate::Location;

pub struct Ast<'a> {
    pub structs: HashMap<&'a str, Struct<'a>>,
    pub funs: Vec<Fun<'a>>,
    pub externs: Vec<Extern<'a>>,
}

pub struct Extern<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub struct Struct<'a> {
    pub fields: Vec<Field<'a>>,
}

pub struct Field<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub enum Item<'a> {
    Fun(Fun<'a>),
    Extern(Extern<'a>),
    Struct(&'a str, Struct<'a>),
}

impl<'a> From<(&'a str, Struct<'a>)> for Item<'a> {
    fn from((name, r#struct): (&'a str, Struct<'a>)) -> Self {
        Self::Struct(name, r#struct)
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
    pub typ: FunType<'a>,
}

#[derive(Clone)]
pub struct FunType<'a> {
    pub params: Vec<Type<'a>>,
    pub ret: Type<'a>,
}

pub enum Literal<'a> {
    Unit,
    Int(i64),
    RawStr(&'a str),
    Str(&'a str),
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

pub struct FieldExpr<'a> {
    pub from: Expr<'a>,
    pub name: &'a str,
    pub name_location: Location<'a>,
}

#[derive(Clone)]
pub struct Lame<'a> {
    pub name: &'a str,
    pub location: Location<'a>,
}

pub enum Expr<'a> {
    Loop(Box<Loop<'a>>),
    New(New<'a>),
    Assign(Box<Assign<'a>>),
    Field(Box<FieldExpr<'a>>),
    Let(Box<Let<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>, Location<'a>),
    If(Box<If<'a>>),
    Var(Lame<'a>),
    Get(Box<Get<'a>>),
    Block(Box<Block<'a>>),
}

impl<'a> From<Loop<'a>> for Expr<'a> {
    fn from(v: Loop<'a>) -> Self {
        Self::Loop(Box::new(v))
    }
}

impl<'a> From<New<'a>> for Expr<'a> {
    fn from(v: New<'a>) -> Self {
        Self::New(v)
    }
}

impl<'a> From<Assign<'a>> for Expr<'a> {
    fn from(v: Assign<'a>) -> Self {
        Self::Assign(Box::new(v))
    }
}

impl<'a> From<FieldExpr<'a>> for Expr<'a> {
    fn from(v: FieldExpr<'a>) -> Self {
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
            Expr::Assign(assign) => assign.location,
            Expr::New(new) => new.location,
            Expr::Loop(loop_expr) => loop_expr.body.ret.location(),
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
            Expr::Assign(_) => true,
            Expr::New(_) => true,
            Expr::Loop(_) => false,
        }
    }
}

impl<'a> From<Lame<'a>> for Expr<'a> {
    fn from(v: Lame<'a>) -> Self {
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
    pub var: Lame<'a>,
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
    Assign(Expr<'a>),
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
    Name(Lame<'a>),
    Fun(Box<FunType<'a>>),
    Prime(Prime),
}

impl<'a> From<Prime> for Type<'a> {
    fn from(v: Prime) -> Self {
        Self::Prime(v)
    }
}

#[derive(Clone, PartialEq)]
pub enum Prime {
    Unit,
    Bool,
    I32,
    U8,
    U64,
}

impl Prime {
    pub fn size(&self) -> u32 {
        match self {
            Prime::Unit => 0,
            Prime::Bool => 1,
            Prime::I32 => 4,
            Prime::U8 => 1,
            Prime::U64 => 8,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Prime::Unit => false,
            Prime::Bool => false,
            Prime::I32 => true,
            Prime::U8 => true,
            Prime::U64 => true,
        }
    }
}

impl Display for Prime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prime::Unit => write!(f, "()"),
            Prime::Bool => write!(f, "bool"),
            Prime::I32 => write!(f, "i32"),
            Prime::U8 => write!(f, "u8"),
            Prime::U64 => write!(f, "u64"),
        }
    }
}

impl<'a> From<Lame<'a>> for Type<'a> {
    fn from(v: Lame<'a>) -> Self {
        Self::Name(v)
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
            Type::Name(n) => Some(n.name),
            _ => None,
        }
    }
}

pub struct Assign<'a> {
    pub expr: Expr<'a>,
    pub to: Expr<'a>,
    pub location: Location<'a>,
}

pub struct New<'a> {
    pub lame: Lame<'a>,
    pub location: Location<'a>,
}

pub struct Loop<'a> {
    pub body: Block<'a>,
}
