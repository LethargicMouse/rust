use std::collections::HashMap;

pub struct Block<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub struct Asg<'a> {
    pub funs: HashMap<&'a str, Fun<'a>>,
}

pub struct Fun<'a> {
    pub params: Vec<&'a str>,
    pub body: Expr<'a>,
}

pub struct If<'a> {
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
}

pub struct Let<'a> {
    pub name: &'a str,
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Let(Box<Let<'a>>),
    Block(Box<Block<'a>>),
    Deref(Box<Expr<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>),
    Var(&'a str),
    If(Box<If<'a>>),
}

impl<'a> From<Let<'a>> for Expr<'a> {
    fn from(v: Let<'a>) -> Self {
        Expr::Let(Box::new(v))
    }
}

impl<'a> From<Block<'a>> for Expr<'a> {
    fn from(v: Block<'a>) -> Self {
        Self::Block(Box::new(v))
    }
}

impl<'a> From<If<'a>> for Expr<'a> {
    fn from(v: If<'a>) -> Self {
        Self::If(Box::new(v))
    }
}

impl<'a> From<Literal<'a>> for Expr<'a> {
    fn from(v: Literal<'a>) -> Self {
        Self::Literal(v)
    }
}

impl<'a> From<Binary<'a>> for Expr<'a> {
    fn from(v: Binary<'a>) -> Self {
        Self::Binary(Box::new(v))
    }
}

impl<'a> From<Call<'a>> for Expr<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
    pub right: Expr<'a>,
}

pub enum BinOp {
    Add,
    Multiply,
    Equal,
    Less,
}

pub struct Call<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}

pub enum Literal<'a> {
    Int(i64),
    Str(&'a str),
}
