use std::collections::HashMap;

pub struct Asg<'a> {
    pub funs: HashMap<&'a str, Fun<'a>>,
}

pub struct Fun<'a> {
    pub params: Vec<&'a str>,
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub struct If<'a> {
    pub condition: Box<Expr<'a>>,
    pub then_expr: Box<Expr<'a>>,
    pub else_expr: Box<Expr<'a>>,
}

pub enum Expr<'a> {
    Deref(Box<Expr<'a>>),
    Call(Call<'a>),
    Binary(Binary<'a>),
    Literal(Literal<'a>),
    Var(&'a str),
    If(If<'a>),
}

impl<'a> From<If<'a>> for Expr<'a> {
    fn from(v: If<'a>) -> Self {
        Self::If(v)
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

impl<'a> From<Call<'a>> for Expr<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
    pub op: BinOp,
}

pub enum BinOp {
    Add,
    Multiply,
    Equal,
}

pub struct Call<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}

pub enum Literal<'a> {
    Int(i64),
    Str(&'a str),
}
