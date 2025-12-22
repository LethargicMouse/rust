use crate::Location;

pub struct Ast<'a> {
    pub funs: Vec<Fun<'a>>,
    pub externs: Vec<Extern<'a>>,
}

pub struct Extern<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub enum Item<'a> {
    Fun(Fun<'a>),
    Extern(&'a str),
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
    pub ret_type: Type<'a>,
}

pub enum Literal<'a> {
    Unit,
    Int(i64),
    RawStr(&'a str),
}

pub struct If<'a> {
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
}

pub struct Block<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub struct Let<'a> {
    pub name: &'a str,
    pub expr: Expr<'a>,
}

pub struct Field<'a> {
    pub from: Expr<'a>,
    pub name: &'a str,
}

pub enum Expr<'a> {
    Field(Box<Field<'a>>),
    Let(Box<Let<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>),
    If(Box<If<'a>>),
    Var(NameLoc<'a>),
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

impl Expr<'_> {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Expr::Call(_) => true,
            Expr::Binary(_) => true,
            Expr::Literal(_) => true,
            Expr::If(if_expr) => {
                if matches!(if_expr.else_expr, Expr::Literal(Literal::Unit)) {
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

impl<'a> From<NameLoc<'a>> for Expr<'a> {
    fn from(v: NameLoc<'a>) -> Self {
        Self::Var(v)
    }
}

impl<'a> From<Get<'a>> for Expr<'a> {
    fn from(v: Get<'a>) -> Self {
        Self::Get(Box::new(v))
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

pub struct Call<'a> {
    pub fun: NameLoc<'a>,
    pub args: Vec<Expr<'a>>,
}

pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
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
    Field(&'a str),
}

impl<'a> From<&'a str> for Postfix<'a> {
    fn from(v: &'a str) -> Self {
        Self::Field(v)
    }
}

impl<'a> From<Call<'a>> for Postfix<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

pub struct Get<'a> {
    pub from: Expr<'a>,
    pub index: Expr<'a>,
}

pub struct NameLoc<'a> {
    pub name: &'a str,
    pub location: Location<'a>,
}

#[derive(Clone)]
pub enum Type<'a> {
    Name(&'a str),
    Fun(Box<FunType<'a>>),
    Unit,
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

impl<'a> Type<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            Type::Name(n) => n,
            Type::Unit => "()",
            Type::Fun(_) => "fn",
        }
    }
}
