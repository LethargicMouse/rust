use std::collections::HashMap;

pub struct Asg<'a> {
    pub funs: HashMap<&'a str, Fun<'a>>,
}

pub struct Fun<'a> {
    pub params: Vec<&'a str>,
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub enum Expr<'a> {
    Call(Call<'a>),
    Binary(Binary<'a>),
    Literal(Literal<'a>),
    Var(&'a str),
}

pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub right: Box<Expr<'a>>,
    pub op: BinOp,
}

pub enum BinOp {
    Add,
}

pub struct Call<'a> {
    pub arg: Box<Expr<'a>>,
    pub name: &'a str,
}

pub enum Literal<'a> {
    Int(i32),
    Str(&'a str),
}
