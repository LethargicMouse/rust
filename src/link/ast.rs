pub struct Ast<'a> {
    pub funs: Vec<(&'a str, Fun<'a>)>,
}

pub struct Fun<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub enum Literal<'a> {
    Unit,
    Int(i32),
    RawStr(&'a str),
}

pub enum Expr<'a> {
    Call(Call<'a>),
    Binary(Binary<'a>),
    Literal(Literal<'a>),
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
    pub arg: Box<Expr<'a>>,
    pub name: &'a str,
}

pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub op: BinOp,
    pub right: Box<Expr<'a>>,
}

pub enum BinOp {
    Plus,
}
