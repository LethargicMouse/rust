pub struct Ast<'a> {
    pub expr: Expr<'a>,
}

pub enum Expr<'a> {
    Unit,
    Call(Call<'a>),
    Int(i32),
    Binary(Binary<'a>),
    RawStr(&'a str),
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
