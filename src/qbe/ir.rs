use std::fmt::Display;

pub struct IR {
    pub types: Vec<TypeDecl>,
    pub consts: Vec<Const>,
    pub funs: Vec<Fun>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in &self.types {
            write!(f, "\ntype :{} = {{", decl.name)?;
            for fields in &decl.variants {
                write!(f, "\n  {{")?;
                for field in fields {
                    write!(f, "{field},")?;
                }
                write!(f, " }}")?;
            }
            write!(f, "\n}}")?;
        }
        for (i, c) in self.consts.iter().enumerate() {
            write!(f, "\ndata $s{} = {{ {c} }}", i + 1)?;
        }
        for fun in &self.funs {
            if fun.name == "main" {
                write!(f, "\nexport")?;
            }
            write!(f, "{fun}")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum Const {
    String(String),
    Struct(Vec<(Unsigned, Value)>),
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::String(s) => write!(f, "b \"{s}\" 0"),
            Const::Struct(values) => {
                for (typ, value) in values {
                    write!(f, "{typ} {value}, ")?;
                }
                Ok(())
            }
        }
    }
}

pub struct Fun {
    pub ret_type: AbiType,
    pub name: String,
    pub params: Vec<(AbiType, Tmp)>,
    pub stmts: Vec<Stmt>,
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\nfunction {} ${}(", self.ret_type, self.name)?;
        for (typ, param) in &self.params {
            write!(f, "{typ} %t{param},")?;
        }
        write!(f, ") {{\n@start")?;
        for stmt in &self.stmts {
            write!(f, "\n  {stmt}")?;
        }
        write!(f, "\n}}")
    }
}

pub type Tmp = u32;

#[derive(Clone, Copy)]
pub enum Type {
    Word,
    Long,
    Float,
    Double,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Word => write!(f, "w"),
            Type::Long => write!(f, "l"),
            Type::Float => write!(f, "s"),
            Type::Double => write!(f, "d"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Signed {
    Base(Type),
    UnsignedByte,
    UnsignedHalf,
    SignedByte,
    SignedHalf,
}

impl From<Type> for Signed {
    fn from(v: Type) -> Self {
        Self::Base(v)
    }
}

impl Display for Signed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Signed::Base(typ) => write!(f, "{typ}"),
            Signed::UnsignedByte => write!(f, "ub"),
            Signed::UnsignedHalf => write!(f, "uh"),
            Signed::SignedByte => write!(f, "sb"),
            Signed::SignedHalf => write!(f, "sh"),
        }
    }
}

#[derive(Clone)]
pub enum DataType {
    Unsigned(Unsigned),
    Name(String),
}

impl From<Unsigned> for DataType {
    fn from(v: Unsigned) -> Self {
        Self::Unsigned(v)
    }
}

impl From<Type> for DataType {
    fn from(typ: Type) -> Self {
        Self::Unsigned(typ.into())
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Unsigned(unsigned) => write!(f, "{unsigned}"),
            DataType::Name(name) => write!(f, ":{name}"),
        }
    }
}

#[derive(Clone)]
pub enum Unsigned {
    Base(Type),
    Half,
    Byte,
}

impl From<Type> for Unsigned {
    fn from(v: Type) -> Self {
        Self::Base(v)
    }
}

impl Display for Unsigned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unsigned::Base(typ) => write!(f, "{typ}"),
            Unsigned::Half => write!(f, "h"),
            Unsigned::Byte => write!(f, "b"),
        }
    }
}

pub enum Stmt {
    Comment(String),
    Stosi(Type, Tmp, Tmp),
    Stoui(Type, Tmp, Tmp),
    Dtosi(Type, Tmp, Tmp),
    Swtof(Type, Tmp, Tmp),
    Exts(Tmp, Tmp),
    Extub(Tmp, Tmp),
    Extsw(Tmp, Tmp),
    Alloc(Tmp, u32, u32),
    Blit(Tmp, Tmp, u32),
    Load(Tmp, Type, Signed, Tmp),
    Ret(Tmp),
    Copy(Tmp, Type, Value),
    Call(Call),
    Bin(Tmp, Type, BinOp, Tmp, Tmp),
    Jnz(Tmp, u16, u16),
    Label(u16),
    Jump(u16),
    Store(Unsigned, Tmp, Tmp),
    Neg(Tmp, Type, Tmp),
}

impl From<Call> for Stmt {
    fn from(v: Call) -> Self {
        Self::Call(v)
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Ret(t) => write!(f, "ret %t{t}"),
            Stmt::Copy(tmp, typ, val) => write!(f, "%t{tmp} ={typ} copy {val}"),
            Stmt::Call(c) => write!(f, "{c}"),
            Stmt::Bin(t, typ, bin_op, l, r) => write!(f, "%t{t} ={typ} {bin_op} %t{l}, %t{r}"),
            Stmt::Jnz(t, r, e) => write!(f, "jnz %t{t}, @l{r}, @l{e}"),
            Stmt::Label(l) => write!(f, "@l{l}"),
            Stmt::Load(tmp, typ, ltyp, l) => write!(f, "%t{tmp} ={typ} load{ltyp} %t{l}"),
            Stmt::Jump(l) => write!(f, "jmp @l{l}"),
            Stmt::Blit(a, b, c) => write!(f, "blit %t{b}, %t{a}, {c}"),
            Stmt::Alloc(t, a, s) => write!(f, "%t{t} =l alloc{a} {s}"),
            Stmt::Store(t, a, b) => write!(f, "store{t} %t{a}, %t{b}"),
            Stmt::Stosi(typ, t, t2) => write!(f, "%t{t} ={typ} stosi %t{t2}"),
            Stmt::Extub(t, t2) => write!(f, "%t{t} =l extub %t{t2}"),
            Stmt::Exts(t, t2) => write!(f, "%t{t} =d exts %t{t2}"),
            Stmt::Dtosi(typ, t, t2) => write!(f, "%t{t} ={typ} dtosi %t{t2}"),
            Stmt::Neg(t, typ, t2) => write!(f, "%t{t} ={typ} neg %t{t2}"),
            Stmt::Comment(c) => write!(f, "# {c}"),
            Stmt::Extsw(t, t2) => write!(f, "%t{t} =l extsw %t{t2}"),
            Stmt::Swtof(typ, t, t2) => write!(f, "%t{t} ={typ} swtof %t{t2}"),
            Stmt::Stoui(typ, t, t2) => write!(f, "%t{t} ={typ} stoui %t{t2}"),
        }
    }
}

pub struct Call {
    pub tmp: Tmp,
    pub ret_type: AbiType,
    pub name: Tmp,
    pub args: Vec<(AbiType, Tmp)>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{} ={} call %t{}(", self.tmp, self.ret_type, self.name)?;
        for (typ, arg) in &self.args {
            write!(f, "{typ} %t{arg},")?;
        }
        write!(f, ")")
    }
}

pub enum BinOp {
    Sub,
    And,
    Add,
    Multiply,
    Equal(Type),
    Less(Type),
    More(Type),
    Inequal,
    Urem,
    Div,
    Or,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Multiply => write!(f, "mul"),
            BinOp::Equal(typ) => write!(f, "ceq{typ}"),
            BinOp::Less(typ) => match typ {
                Type::Float | Type::Double => write!(f, "clt{typ}"),
                _ => write!(f, "cslt{typ}"),
            },
            BinOp::More(typ) => match typ {
                Type::Float | Type::Double => write!(f, "cgt{typ}"),
                _ => write!(f, "csgt{typ}"),
            },
            BinOp::Inequal => write!(f, "cnel"),
            BinOp::Urem => write!(f, "urem"),
            BinOp::Div => write!(f, "div"),
            BinOp::And => write!(f, "and"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f32),
    Double(f64),
    Const(u16),
    Fun(String),
    Tmp(Tmp),
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Const(n) => write!(f, "$s{n}"),
            Value::Tmp(t) => write!(f, "%t{t}"),
            Value::Float(n) => write!(f, "s_{n}"),
            Value::Double(n) => write!(f, "d_{n}"),
            Value::Fun(fun) => write!(f, "${fun}"),
        }
    }
}

pub enum AbiType {
    Signed(Signed),
    Name(String),
}

impl From<Signed> for AbiType {
    fn from(v: Signed) -> Self {
        Self::Signed(v)
    }
}

impl From<Type> for AbiType {
    fn from(v: Type) -> Self {
        Self::Signed(v.into())
    }
}

impl Display for AbiType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AbiType::Signed(t) => write!(f, "{t}"),
            AbiType::Name(n) => write!(f, ":{n}"),
        }
    }
}

pub struct TypeDecl {
    pub name: String,
    pub variants: Vec<Vec<DataType>>,
}
