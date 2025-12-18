pub struct Ast<'a> {
    pub funs: Vec<(&'a str, Fun<'a>)>,
}

pub struct Fun<'a> {
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
}

pub enum Expr<'a> {
    Call(Call<'a>),
    Binary(Binary<'a>),
    Literal(Literal<'a>),
    If(If<'a>),
    Var(&'a str),
    Get(Get<'a>),
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
    pub name: &'a str,
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
