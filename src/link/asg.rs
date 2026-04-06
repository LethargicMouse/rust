use std::collections::HashMap;

use crate::link::analyse::Info;

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub ret: Expr<'a>,
}

pub struct Struct<'a> {
    pub generics: Vec<&'a str>,
    pub variants: Vec<Vec<Type<'a>>>,
}

pub struct Impl<'a> {
    pub generics: Vec<&'a str>,
    pub trait_generics: Vec<Type<'a>>,
    pub typ: Type<'a>,
    pub fun: Fun<'a>,
}

pub struct Asg<'a> {
    pub funs: HashMap<&'a str, Fun<'a>>,
    pub trait_funs: HashMap<&'a str, (usize, Vec<Impl<'a>>)>,
    pub structs: HashMap<&'a str, Struct<'a>>,
    pub info: Info<'a>,
    pub consts: HashMap<&'a str, (Type<'a>, Expr<'a>)>,
}

pub struct Fun<'a> {
    pub params: Vec<(&'a str, Type<'a>)>,
    pub ret_type: Type<'a>,
    pub body: Expr<'a>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug)]
pub struct Let<'a> {
    pub name: &'a str,
    pub expr: Expr<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub from: Expr<'a>,
    pub id: usize,
    pub typ: Type<'a>,
    pub from_type: Type<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    FunRef(FunRef<'a>),
    Expr(Box<Match<'a>>),
    Negate(Box<Negate<'a>>),
    Cast(Box<Cast<'a>>),
    Break(Box<Break<'a>>),
    Return(Box<Return<'a>>),
    Ref(Box<Ref<'a>>),
    Loop(Box<Loop<'a>>),
    Tuple(Tuple<'a>),
    Assign(Box<Assign<'a>>),
    Field(Box<Field<'a>>),
    Let(Box<Let<'a>>),
    Block(Box<Block<'a>>),
    Deref(Box<Deref<'a>>),
    Call(Box<Call<'a>>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>),
    Var(&'a str),
    If(Box<If<'a>>),
}

impl<'a> From<FunRef<'a>> for Expr<'a> {
    fn from(v: FunRef<'a>) -> Self {
        Self::FunRef(v)
    }
}

impl<'a> From<Match<'a>> for Expr<'a> {
    fn from(v: Match<'a>) -> Self {
        Self::Expr(Box::new(v))
    }
}

impl<'a> From<Negate<'a>> for Expr<'a> {
    fn from(v: Negate<'a>) -> Self {
        Self::Negate(Box::new(v))
    }
}

impl<'a> From<Cast<'a>> for Expr<'a> {
    fn from(v: Cast<'a>) -> Self {
        Self::Cast(Box::new(v))
    }
}

impl<'a> From<Break<'a>> for Expr<'a> {
    fn from(v: Break<'a>) -> Self {
        Self::Break(Box::new(v))
    }
}

impl<'a> Default for Expr<'a> {
    fn default() -> Self {
        Self::Literal(Literal::Int(0, Type::I32))
    }
}

impl<'a> From<Deref<'a>> for Expr<'a> {
    fn from(v: Deref<'a>) -> Self {
        Self::Deref(Box::new(v))
    }
}

impl<'a> From<Return<'a>> for Expr<'a> {
    fn from(v: Return<'a>) -> Self {
        Self::Return(Box::new(v))
    }
}

impl<'a> From<Ref<'a>> for Expr<'a> {
    fn from(v: Ref<'a>) -> Self {
        Self::Ref(Box::new(v))
    }
}

impl<'a> From<Loop<'a>> for Expr<'a> {
    fn from(v: Loop<'a>) -> Self {
        Self::Loop(Box::new(v))
    }
}

impl<'a> From<Tuple<'a>> for Expr<'a> {
    fn from(v: Tuple<'a>) -> Self {
        Self::Tuple(v)
    }
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
        Self::Call(Box::new(v))
    }
}

#[derive(Debug)]
pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
    pub right: Expr<'a>,
    pub args_typ: Type<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug)]
pub enum BinOp {
    Subtract,
    And,
    Add,
    Multiply,
    Equal,
    Less,
    More,
    NotEqual,
    Modulo,
    Divide,
    Or,
}

#[derive(Debug)]
pub struct Call<'a> {
    pub expr: Expr<'a>,
    pub args: Vec<(Type<'a>, Expr<'a>)>,
    pub ret_type: Type<'a>,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Int(i64, Type<'a>),
    Str(&'a str),
    SizeOf(Type<'a>),
}

#[derive(Debug)]
pub struct Assign<'a> {
    pub expr: Expr<'a>,
    pub expr_type: Type<'a>,
    pub to: Expr<'a>,
}

#[derive(Debug)]
pub struct Tuple<'a> {
    pub exprs: Vec<(Type<'a>, Expr<'a>)>,
}

#[derive(Debug)]
pub struct Loop<'a> {
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Ref<'a> {
    pub expr: Expr<'a>,
    pub expr_typ: Type<'a>,
}

#[derive(Debug)]
pub struct Return<'a> {
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct Break<'a> {
    pub expr: Expr<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug)]
pub struct Deref<'a> {
    pub expr: Expr<'a>,
    pub typ: Type<'a>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Type<'a> {
    Name(&'a str, Vec<Type<'a>>),
    Ref(Box<Type<'a>>),
    Ptr(Box<Type<'a>>),
    FunPtr(Vec<Type<'a>>),
    Cold(usize),
    Generic(&'a str),
    U8,
    U64,
    #[default]
    Unknown,
    Unit,
    I32,
    I64,
    F32,
    F64,
    Bool,
}

impl Type<'_> {
    pub fn is_i(&self) -> bool {
        matches!(self, Type::I32 | Type::I64 | Type::Bool)
    }

    pub fn is_f(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_u(&self) -> bool {
        matches!(self, Type::U8 | Type::U64)
    }
}

#[derive(Debug)]
pub struct Cast<'a> {
    pub expr: Expr<'a>,
    pub from: Type<'a>,
    pub to: Type<'a>,
}

#[derive(Debug)]
pub struct Negate<'a> {
    pub expr: Expr<'a>,
    pub expr_typ: Type<'a>,
}

#[derive(Debug)]
pub struct Match<'a> {
    pub expr: Expr<'a>,
    pub expr_typ: Type<'a>,
    pub typ: Type<'a>,
    pub pattern_matches: Vec<PatternMatch<'a>>,
}

#[derive(Debug)]
pub struct PatternMatch<'a> {
    pub label: u8,
    pub mtyp: Option<(&'a str, Type<'a>)>,
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub struct FunRef<'a> {
    pub name: &'a str,
    pub generics: Vec<(&'a str, Type<'a>)>,
}
