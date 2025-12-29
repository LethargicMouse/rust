use std::fmt::Display;

pub struct IR {
    pub consts: Vec<Const>,
    pub funs: Vec<Fun>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    pub name: String,
    pub params: Vec<Tmp>,
    pub stmts: Vec<Stmt>,
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\nfunction w ${}(", self.name)?;
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

pub enum Stmt {
    Load(Tmp, Tmp),
    Ret(Tmp),
    Copy(Tmp, Type, Value),
    Call(Call),
    Bin(Tmp, BinOp, Tmp, Tmp),
    Jnz(Tmp, u16, u16),
    Label(u16),
    Jump(u16),
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
            Stmt::Load(t, l) => write!(f, "%t{t} =l loadl %t{l}"),
            Stmt::Jump(l) => write!(f, "jmp @l{l}"),
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
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Multiply => write!(f, "mul"),
            BinOp::Equal => write!(f, "ceql"),
            BinOp::Less => write!(f, "csltl"),
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
