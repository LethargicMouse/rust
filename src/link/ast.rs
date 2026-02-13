use std::{collections::HashMap, fmt::Display};

use crate::Location;

pub struct Ast<'a> {
    pub type_aliases: HashMap<&'a str, Type<'a>>,
    pub end: Location<'a>,
    pub structs: HashMap<&'a str, Struct<'a>>,
    pub funs: Vec<Fun<'a>>,
    pub externs: Vec<Extern<'a>>,
    pub traits: HashMap<&'a str, Trait<'a>>,
    pub impls: Vec<Impl<'a>>,
    pub consts: Vec<Const<'a>>,
}

pub struct Const<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
    pub expr: Expr<'a>,
}

pub struct Impl<'a> {
    pub lame: Lame<'a>,
    pub typ: Type<'a>,
    pub funs: Vec<Fun<'a>>,
}

pub struct Extern<'a> {
    pub name: &'a str,
    pub typ: FunType<'a>,
}

pub struct Struct<'a> {
    pub generics: Vec<Generic<'a>>,
    pub fields: Vec<Field<'a>>,
}

pub struct Field<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub enum Item<'a> {
    Const(Const<'a>),
    Impl(Impl<'a>),
    Trait(&'a str, Trait<'a>),
    TypeAlias(TypeAlias<'a>),
    Fun(Box<Fun<'a>>),
    Extern(Extern<'a>),
    Struct(&'a str, Struct<'a>),
}

impl<'a> From<Const<'a>> for Item<'a> {
    fn from(v: Const<'a>) -> Self {
        Self::Const(v)
    }
}

impl<'a> From<Impl<'a>> for Item<'a> {
    fn from(v: Impl<'a>) -> Self {
        Self::Impl(v)
    }
}

impl<'a> From<(&'a str, Trait<'a>)> for Item<'a> {
    fn from((name, trait_): (&'a str, Trait<'a>)) -> Self {
        Self::Trait(name, trait_)
    }
}

impl<'a> From<TypeAlias<'a>> for Item<'a> {
    fn from(v: TypeAlias<'a>) -> Self {
        Self::TypeAlias(v)
    }
}

impl<'a> From<(&'a str, Struct<'a>)> for Item<'a> {
    fn from((name, r#struct): (&'a str, Struct<'a>)) -> Self {
        Self::Struct(name, r#struct)
    }
}

impl<'a> From<Extern<'a>> for Item<'a> {
    fn from(v: Extern<'a>) -> Self {
        Self::Extern(v)
    }
}

impl<'a> From<Fun<'a>> for Item<'a> {
    fn from(v: Fun<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

pub struct Fun<'a> {
    pub header: Header<'a>,
    pub body: Expr<'a>,
}

pub struct Header<'a> {
    pub lame: Lame<'a>,
    pub params: Vec<&'a str>,
    pub typ: FunType<'a>,
}

#[derive(Clone)]
pub struct FunType<'a> {
    pub generics: Vec<Generic<'a>>,
    pub params: Vec<Type<'a>>,
    pub ret: Type<'a>,
    pub location: Location<'a>,
}

pub enum Literal<'a> {
    Unit,
    Int(i64),
    Str(&'a str),
    Bool(bool),
    Size(Type<'a>),
}

pub struct If<'a> {
    pub location: Location<'a>,
    pub condition: Expr<'a>,
    pub then_expr: Expr<'a>,
    pub else_expr: Expr<'a>,
}

pub struct Block<'a> {
    pub stmts: Vec<Expr<'a>>,
    pub last_semi_location: Option<Location<'a>>,
    pub ret: Expr<'a>,
}

impl<'a> From<Expr<'a>> for Block<'a> {
    fn from(ret: Expr<'a>) -> Self {
        Block {
            stmts: Vec::new(),
            last_semi_location: None,
            ret,
        }
    }
}

pub struct Let<'a> {
    pub location: Location<'a>,
    pub name: &'a str,
    pub typ: Option<Type<'a>>,
    pub expr: Expr<'a>,
}

pub struct FieldExpr<'a> {
    pub expr: Expr<'a>,
    pub name: &'a str,
    pub name_location: Location<'a>,
}

#[derive(Clone, Copy)]
pub struct Lame<'a> {
    pub name: &'a str,
    pub location: Location<'a>,
}

pub enum Expr<'a> {
    Break(Box<Break<'a>>),
    ImplicitUnit(Location<'a>),
    Array(Array<'a>),
    Cast(Box<Cast<'a>>),
    Return(Box<Return<'a>>),
    Ref(Box<Ref<'a>>),
    Loop(Box<Loop<'a>>),
    New(New<'a>),
    Assign(Box<Assign<'a>>),
    Field(Box<FieldExpr<'a>>),
    Let(Box<Let<'a>>),
    Call(Call<'a>),
    Binary(Box<Binary<'a>>),
    Literal(Literal<'a>, Location<'a>),
    If(Box<If<'a>>),
    Var(Lame<'a>),
    Get(Box<Get<'a>>),
    Block(Box<Block<'a>>),
}

impl<'a> From<Break<'a>> for Expr<'a> {
    fn from(v: Break<'a>) -> Self {
        Self::Break(Box::new(v))
    }
}

impl<'a> From<Array<'a>> for Expr<'a> {
    fn from(v: Array<'a>) -> Self {
        Self::Array(v)
    }
}

impl<'a> From<Call<'a>> for Expr<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

impl<'a> From<Cast<'a>> for Expr<'a> {
    fn from(v: Cast<'a>) -> Self {
        Self::Cast(Box::new(v))
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

impl<'a> From<New<'a>> for Expr<'a> {
    fn from(v: New<'a>) -> Self {
        Self::New(v)
    }
}

impl<'a> From<Assign<'a>> for Expr<'a> {
    fn from(v: Assign<'a>) -> Self {
        Self::Assign(Box::new(v))
    }
}

impl<'a> From<FieldExpr<'a>> for Expr<'a> {
    fn from(v: FieldExpr<'a>) -> Self {
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

impl<'a> Expr<'a> {
    pub fn location(&self) -> Location<'a> {
        match self {
            Expr::Field(field) => field.name_location,
            Expr::Let(let_expr) => let_expr.location,
            Expr::Call(call) => call.lame.location,
            Expr::Binary(binary) => binary.location,
            Expr::Literal(_, location) => *location,
            Expr::If(if_expr) => if_expr.location,
            Expr::Var(var) => var.location,
            Expr::Get(get) => get.location,
            Expr::Block(block) => block.ret.location(),
            Expr::Assign(assign) => assign.location,
            Expr::New(new) => new.lame.location,
            Expr::Loop(loop_expr) => loop_expr.body.location(),
            Expr::Ref(ref_expr) => ref_expr.location,
            Expr::Return(ret) => ret.location,
            Expr::Cast(cast) => cast.location,
            Expr::Array(array) => array.location,
            Expr::ImplicitUnit(location) => *location,
            Expr::Break(break_expr) => break_expr.location,
        }
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            Expr::Call(_) => true,
            Expr::Binary(_) => true,
            Expr::Literal(_, _) => true,
            Expr::If(if_expr) => {
                if matches!(if_expr.else_expr, Expr::ImplicitUnit(_)) {
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
            Expr::Assign(_) => true,
            Expr::New(_) => true,
            Expr::Loop(loop_expr) => loop_expr.body.needs_semicolon(),
            Expr::Ref(_) => true,
            Expr::Return(_) => true,
            Expr::Cast(_) => true,
            Expr::Array(_) => true,
            Expr::ImplicitUnit(_) => true,
            Expr::Break(_) => true,
        }
    }
}

impl<'a> From<Lame<'a>> for Expr<'a> {
    fn from(v: Lame<'a>) -> Self {
        Self::Var(v)
    }
}

impl<'a> From<Get<'a>> for Expr<'a> {
    fn from(v: Get<'a>) -> Self {
        Self::Get(Box::new(v))
    }
}

impl<'a> From<Binary<'a>> for Expr<'a> {
    fn from(v: Binary<'a>) -> Self {
        Self::Binary(Box::new(v))
    }
}

pub struct Call<'a> {
    pub lame: Lame<'a>,
    pub args: Vec<Expr<'a>>,
}

pub struct Binary<'a> {
    pub left: Expr<'a>,
    pub op: BinOp,
    pub location: Location<'a>,
    pub right: Expr<'a>,
}

#[derive(Clone, Copy)]
pub enum BinOp {
    Multiply,
    Subtract,
    Plus,
    Equal,
    Less,
    NotEqual,
    Mod,
    Div,
    And,
}

impl BinOp {
    pub fn priority(&self) -> u8 {
        match self {
            BinOp::And => 0,
            BinOp::Equal | BinOp::Less | BinOp::NotEqual => 1,
            BinOp::Plus | BinOp::Subtract => 2,
            BinOp::Mod | BinOp::Div | BinOp::Multiply => 3,
        }
    }
}

pub enum Postfix<'a> {
    Assign(Expr<'a>),
    Get(Expr<'a>),
    Call(Call<'a>),
    Field(&'a str, Location<'a>),
    Cast(Type<'a>),
}

impl<'a> From<Call<'a>> for Postfix<'a> {
    fn from(v: Call<'a>) -> Self {
        Self::Call(v)
    }
}

pub struct Get<'a> {
    pub from: Expr<'a>,
    pub index: Expr<'a>,
    pub location: Location<'a>,
}

#[derive(Clone)]
pub enum Type<'a> {
    Ref(Box<Type<'a>>, Location<'a>),
    Ptr(Box<Type<'a>>, Location<'a>),
    Name(Lame<'a>, Vec<Type<'a>>),
    Fun(Box<FunType<'a>>),
    Prime(Prime, Location<'a>),
}

impl<'a> Type<'a> {
    pub fn location(&self) -> Location<'a> {
        match self {
            Type::Ptr(_, location) => *location,
            Type::Name(lame, _) => lame.location,
            Type::Fun(fun_type) => fun_type.location,
            Type::Prime(_, location) => *location,
            Type::Ref(_, location) => *location,
        }
    }

    pub fn name(lame: Lame<'a>) -> Self {
        Self::Name(lame, Vec::new())
    }

    pub fn get_name(&self) -> &'a str {
        match self {
            Type::Ptr(typ, _) => typ.get_name(),
            Type::Name(lame, _) => lame.name,
            Type::Fun(_) => "fun",
            Type::Prime(prime, _) => prime.get_name(),
            Type::Ref(typ, _) => typ.get_name(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Prime {
    Unit,
    Bool,
    I32,
    I64,
    U8,
    U64,
    F32,
    F64,
}

impl Prime {
    fn get_name(&self) -> &'static str {
        match self {
            Prime::Unit => "unit",
            Prime::Bool => "bool",
            Prime::I32 => "i32",
            Prime::I64 => "i64",
            Prime::U8 => "u8",
            Prime::U64 => "u64",
            Prime::F32 => "f32",
            Prime::F64 => "f64",
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "i32" => Some(Prime::I32),
            "i64" => Some(Prime::I64),
            "u8" => Some(Prime::U8),
            "u64" => Some(Prime::U64),
            "bool" => Some(Prime::Bool),
            "f32" => Some(Prime::F32),
            "f64" => Some(Prime::F64),
            _ => None,
        }
    }

    pub fn size(&self) -> u32 {
        match self {
            Prime::Unit => 0,
            Prime::Bool => 1,
            Prime::I32 => 4,
            Prime::U8 => 1,
            Prime::U64 => 8,
            Prime::I64 => 8,
            Prime::F32 => 4,
            Prime::F64 => 8,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Prime::Unit => false,
            Prime::Bool => false,
            Prime::I32 => true,
            Prime::U8 => true,
            Prime::U64 => true,
            Prime::I64 => true,
            Prime::F32 => true,
            Prime::F64 => true,
        }
    }
}

impl Display for Prime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl<'a> From<Lame<'a>> for Type<'a> {
    fn from(v: Lame<'a>) -> Self {
        if let Some(prime) = Prime::from_name(v.name) {
            Self::Prime(prime, v.location)
        } else {
            Self::name(v)
        }
    }
}

impl<'a> From<FunType<'a>> for Type<'a> {
    fn from(v: FunType<'a>) -> Self {
        Self::Fun(Box::new(v))
    }
}

pub struct Assign<'a> {
    pub expr: Expr<'a>,
    pub to: Expr<'a>,
    pub location: Location<'a>,
}

pub struct New<'a> {
    pub lame: Lame<'a>,
    pub fields: Vec<NewField<'a>>,
}

pub struct Loop<'a> {
    pub body: Expr<'a>,
}

pub struct Ref<'a> {
    pub expr: Expr<'a>,
    pub location: Location<'a>,
}

pub struct Return<'a> {
    pub expr: Expr<'a>,
    pub location: Location<'a>,
}

pub struct Break<'a> {
    pub expr: Expr<'a>,
    pub location: Location<'a>,
}

pub struct Cast<'a> {
    pub expr: Expr<'a>,
    pub typ: Type<'a>,
    pub location: Location<'a>,
}

pub struct Array<'a> {
    pub elems: Vec<Expr<'a>>,
    pub location: Location<'a>,
}

pub struct NewField<'a> {
    pub lame: Lame<'a>,
    pub expr: Expr<'a>,
}

pub struct TypeAlias<'a> {
    pub name: &'a str,
    pub typ: Type<'a>,
}

pub struct Trait<'a> {
    pub headers: Vec<Header<'a>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Generic<'a> {
    pub name: &'a str,
    pub constraint: Option<&'a str>,
}
