use crate::Location;

pub struct Ast<'a> {
    pub funs: Vec<Fun<'a>>,
    pub externs: Vec<&'a str>,
}

pub enum Item<'a> {
    Fun(Fun<'a>),
    Extern(&'a str),
}

pub struct Fun<'a> {
    pub name: &'a str,
    pub params: Vec<&'a str>,
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub enum Literal<'a> {
    Unit,
    Int(i64),
    RawStr(&'a str),
}

pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub then_expr: Box<Expr<'a>>,
    pub else_expr: Box<Expr<'a>>,
}

pub enum Expr<'a> {
    Call(Call<'a>),
    Binary(Binary<'a>),
    Literal(Literal<'a>),
    If(If<'a>),
    Var(NameLoc<'a>),
    Get(Get<'a>),
}

impl<'a> From<NameLoc<'a>> for Expr<'a> {
    fn from(v: NameLoc<'a>) -> Self {
        Self::Var(v)
    }
}

impl<'a> From<Get<'a>> for Expr<'a> {
    fn from(v: Get<'a>) -> Self {
        Self::Get(v)
    }
}

impl<'a> From<Literal<'a>> for Expr<'a> {
    fn from(v: Literal<'a>) -> Self {
        Self::Literal(v)
    }
}

impl<'a> From<Binary<'a>> for Expr<'a> {
    fn from(v: Binary<'a>) -> Self {
        Self::Binary(v)
    }
}

pub struct Call<'a> {
    pub fun: NameLoc<'a>,
    pub args: Vec<Expr<'a>>,
}

pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub op: BinOp,
    pub right: Box<Expr<'a>>,
}

pub enum BinOp {
    Plus,
    Equal,
}

pub enum Postfix<'a> {
    Get(Expr<'a>),
}

pub struct Get<'a> {
    pub from: Box<Expr<'a>>,
    pub index: Box<Expr<'a>>,
}

pub struct NameLoc<'a> {
    pub name: &'a str,
    pub location: Location<'a>,
}
