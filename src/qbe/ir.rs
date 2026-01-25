use std::fmt::Display;

pub struct IR {
    pub types: Vec<TypeDecl>,
    pub consts: Vec<Const>,
    pub funs: Vec<Fun>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for decl in &self.types {
            write!(f, "\ntype :{} = {{ ", decl.name)?;
            for field in &decl.fields {
                write!(f, "{field},")?;
            }
            write!(f, "}}")?;
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

pub enum Const {
    String(String),
    Struct(Vec<Value>),
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::String(s) => write!(f, "b \"{s}\" 0"),
            Const::Struct(values) => {
                for value in values {
                    write!(f, "l {value}, ")?;
                }
                Ok(())
            }
        }
    }
}

pub struct Fun {
    pub ret_type: AbiType,
    pub name: String,
    pub params: Vec<Tmp>,
    pub stmts: Vec<Stmt>,
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\nfunction {} ${}(", self.ret_type, self.name)?;
        for param in &self.params {
            write!(f, "l %t{param},")?;
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
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Word => write!(f, "w"),
            Type::Long => write!(f, "l"),
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

impl Signed {
    fn fit_in_base(self) -> Type {
        match self {
            Signed::Base(typ) => typ,
            _ => Type::Word,
        }
    }
}

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
    Alloc(Tmp, u32, u32),
    Blit(Tmp, Tmp, u32),
    Load(Tmp, Signed, Tmp),
    Ret(Tmp),
    Copy(Tmp, Type, Value),
    Call(Call),
    Bin(Tmp, BinOp, Tmp, Tmp),
    Jnz(Tmp, u16, u16),
    Label(u16),
    Jump(u16),
    Store(Unsigned, Tmp, Tmp),
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
            Stmt::Bin(t, bin_op, l, r) => write!(f, "%t{t} =l {bin_op} %t{l}, %t{r}"),
            Stmt::Jnz(t, r, e) => write!(f, "jnz %t{t}, @l{r}, @l{e}"),
            Stmt::Label(l) => write!(f, "@l{l}"),
            Stmt::Load(tmp, typ, l) => write!(f, "%t{tmp} ={} load{typ} %t{l}", typ.fit_in_base()),
            Stmt::Jump(l) => write!(f, "jmp @l{l}"),
            Stmt::Blit(a, b, c) => write!(f, "blit %t{a}, %t{b}, {c}"),
            Stmt::Alloc(t, a, s) => write!(f, "%t{t} =l alloc{a} {s}"),
            Stmt::Store(t, a, b) => write!(f, "store{t} %t{a}, %t{b}"),
        }
    }
}

pub struct Call {
    pub tmp: Tmp,
    pub name: String,
    pub args: Vec<Tmp>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{} =l call ${}(", self.tmp, self.name)?;
        for arg in &self.args {
            write!(f, "l %t{arg},")?;
        }
        write!(f, ")")
    }
}

pub enum BinOp {
    Add,
    Multiply,
    Equal,
    Less,
    Inequal,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Multiply => write!(f, "mul"),
            BinOp::Equal => write!(f, "ceql"),
            BinOp::Less => write!(f, "csltl"),
            BinOp::Inequal => write!(f, "cnel"),
        }
    }
}

pub enum Value {
    Int(i64),
    Const(u16),
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
    pub fields: Vec<AbiType>,
}
