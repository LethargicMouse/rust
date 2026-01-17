use std::collections::HashMap;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct If<'a> {
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
}

#[derive(Debug)]
pub struct Let<'a> {
    pub name: &'a str,
    pub expr: Expr<'a>,
    pub expr_align: u32,
    pub expr_size: u32,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub from: Expr<'a>,
    pub offset: u32,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Assign(Box<Assign<'a>>),
    Field(Box<Field<'a>>),
    Let(Box<Let<'a>>),
    Block(Box<Block<'a>>),
    Deref(Box<Expr<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>),
    Var(&'a str),
    If(Box<If<'a>>),
}

impl<'a> From<Assign<'a>> for Expr<'a> {
    fn from(v: Assign<'a>) -> Self {
        Self::Assign(Box::new(v))
    }
}

impl<'a> From<&'a str> for Expr<'a> {
    fn from(v: &'a str) -> Self {
        Self::Var(v)
    }
}

impl<'a> From<Field<'a>> for Expr<'a> {
    fn from(v: Field<'a>) -> Self {
        Self::Field(Box::new(v))
    }
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

#[derive(Debug)]
pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
    pub right: Expr<'a>,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Multiply,
    Equal,
    Less,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Int(i64),
    RawStr(&'a str),
    Str(&'a str),
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub expr: Expr<'a>,
    pub expr_size: u32,
    pub to: Expr<'a>,
}
